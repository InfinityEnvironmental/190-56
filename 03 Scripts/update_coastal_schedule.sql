-- Script to create update or create new schedule for coastal water quality samples

BEGIN;

WITH schedule_insert AS
(WITH schedule AS
(SELECT
	sample_date::date,
	trim(initcap(to_char(sample_date, 'day')))::varchar(9) AS day,
	((date_trunc('week', sample_date)::date - '2026-03-09') / 7 + 1)::varchar(1) AS week
FROM generate_series('2026-03-09'::date, '2026-4-03'::date, '1 day'::interval) sample_date) -- Change start and end dates
SELECT
	a.sample_date,
	a.week,
	lower(a.day)::coastal.weekday AS day,
	b.site_id,
	CASE
		WHEN b.samplers = 'Coastal Management Branch' THEN 'cmb'::coastal.branch
		WHEN b.samplers = 'Scientific Services Branch' THEN 'ssb'::coastal.branch
		END AS samplers
FROM schedule a JOIN coastal.schedule b ON a.week = b.week AND a.day = b.day
WHERE version = 5 AND a.day NOT IN ('Saturday', 'Sunday')) -- Which version of the schedule do I want to use as a template
INSERT INTO coastal.schedule_planned (date, week, day, site_id, samplers)
SELECT * FROM schedule_insert;

SELECT * FROM coastal.planned_schedule_view
WHERE date >= '2026-03-09'
ORDER BY date, samplers, site_id;

SELECT * FROM coastal.planned_schedule_view WHERE date >= '2026-03-09';

ROLLBACK;
COMMIT;

SELECT * FROM strandfontein.water_samples;

ALTER TABLE strandfontein.water_samples ADD COLUMN lab_duplicate boolean DEFAULT false NOT NULL;

