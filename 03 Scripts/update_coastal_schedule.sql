-- Script to create update or create new schedule for coastal water quality samples

BEGIN;

DELETE FROM coastal.schedule_planned;

WITH schedule_insert AS
(WITH schedule AS
(SELECT
	sample_date::date,
	trim(initcap(to_char(sample_date, 'day')))::varchar(9) AS day,
	((date_trunc('week', sample_date)::date - '2025-04-07') / 7 + 1)::varchar(1) AS week
FROM generate_series('2025-04-07'::date, '2025-05-02'::date, '1 day'::interval) sample_date) -- Change start and end dates
SELECT
	a.sample_date,
	a.week,
	lower(a.day)::coastal.weekday AS day,
	b.site_id,
	CASE
		WHEN b.samplers = 'Coastal Management Branch' THEN 'cmb'::coastal.branch
		WHEN b.samplers = 'Scientific Services Branch' THEN 'ssb'::coastal.branch
		END AS samplers
FROM schedule a LEFT JOIN coastal.schedule b ON a.week = b.week AND a.day = b.day
WHERE version = 3) -- Which version of the schedule do I want to use as a template
INSERT INTO coastal.schedule_planned (date, week, day, site_id, samplers)
SELECT * FROM schedule_insert;

SELECT * FROM coastal.schedule_planned;

ROLLBACK;
COMMIT;

-- Make changes to coastal schedule

-- Monday moves to Tuesday
-- Tuesday moves to Wednesday CMB
-- Wednesday CMB moves to Thursday
-- Wednesday SSB remains the same
-- Thursday moves to Friday
-- Friday stays on Friday

BEGIN;

SELECT * FROM coastal.schedule_planned
WHERE date = '2025-04-21' AND samplers = 'ssb';

UPDATE coastal.schedule_planned
SET date = '2025-04-22', day = 'tuesday', samplers = 'ssb'
WHERE date = '2025-04-21' AND samplers = 'ssb';

SAVEPOINT thursday;
SAVEPOINT wednesday;
SAVEPOINT tuesday;
SAVEPOINT monday;

SELECT * FROM coastal.schedule_planned WHERE week = '3' ORDER BY day

SAVEPOINT thursday;
SAVEPOINT wednesday_cmb;
SAVEPOINT tuesday;

ROLLBACK;

SELECT * FROM coastal.schedule;
CREATE TYPE coastal.weekday AS enum ('monday', 'tuesday', 'wednesday', 'thursday', 'friday');
CREATE TYPE coastal.branch AS enum ('cmb', 'ssb');

BEGIN;

ALTER TABLE coastal.schedule_planned ADD COLUMN day_enum coastal.weekday;
ALTER TABLE coastal.schedule_planned ADD COLUMN samplers_enum coastal.branch;

UPDATE coastal.schedule_planned
SET samplers_enum = CASE
	WHEN samplers = 'Coastal Management Branch' THEN 'cmb'::coastal.branch
	WHEN samplers = 'Scientific Services Branch' THEN 'ssb'::coastal.branch END;

SELECT * FROM coastal.schedule_planned;

ALTER TABLE coastal.schedule_planned DROP COLUMN day;
ALTER TABLE coastal.schedule_planned DROP COLUMN samplers;

ALTER TABLE coastal.schedule_planned RENAME COLUMN day_enum TO day;
ALTER TABLE coastal.schedule_planned RENAME COLUMN samplers_enum TO samplers;

SELECT * FROM coastal.schedule_planned;

ALTER TABLE coastal.schedule_planned ALTER COLUMN day TYPE coastal.weekday;

ROLLBACK;
COMMIT;
