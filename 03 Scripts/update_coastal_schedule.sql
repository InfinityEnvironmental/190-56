BEGIN;

WITH turbidity AS (SELECT
	a.label,
	'Turbidity' AS parameter,
	'NTU' AS unit,
	a.numeric_value
FROM (VALUES
	('W31', 25.6),
	('W32', 22.5),
	('W33', 34.8),
	('W34', 44.0),
	('W35', 26.3),
	('W36', 41.3),
	('W37', 66.1),
	('W38', 110),
	('W39', 29.8),
	('W40', 24.4),
	('W41', 30.0),
	('W42', 23.7),
	('W43', 26.4)
	) a(label, numeric_value))
INSERT INTO strandfontein.water_results_insitu (result_id, parameter, mean_value, unit)
SELECT
	b.result_id,
	c.parameter,
	c.numeric_value,
	c.unit
FROM strandfontein.water_samples a
	JOIN strandfontein.water_results b ON a.sample_id = b.sample_id
	JOIN turbidity c ON a.label = c.label AND b.suite = 'insitu';

SELECT * FROM strandfontein.water_samples a
	JOIN strandfontein.water_results b USING (sample_id)
	JOIN strandfontein.water_results_insitu c USING (result_id)
WHERE a.sample_date = '2026-05-22' AND c.parameter = 'Turbidity';

ROLLBACK;
COMMIT;

SELECT * FROM strandfontein.water_insitu_view;

SELECT * FROM strandfontein.water_results_insitu;
ALTER TABLE strandfontein.water_results_insitu ALTER COLUMN filename DROP NOT NULL;

