-- Script to create views for coastal water quality dashboard

BEGIN;

-- Drop the current views

DROP VIEW coastal.most_recent_results;
DROP VIEW coastal.rolling365day;
DROP VIEW coastal.rolling5year;
DROP VIEW coastal.results_view;

-- Create a new results view

CREATE VIEW coastal.results_view AS
SELECT
	CASE
		WHEN site_id = 'XCS16' THEN 'CS41' -- Fish Hoek Beach
		WHEN site_id = 'ICS14' THEN 'CN16O' -- Saunders Rocks
		WHEN site_id = 'ICS12' THEN 'CN11' -- Camps Bay Lifesaving Tower
		WHEN site_id = 'ICS10' THEN 'CN41' -- Camps Bay South
		WHEN site_id = 'ICS15' THEN 'CN05' -- Green Point Pump Station / Mouille Point
		WHEN site_id = 'ICS01' THEN 'CN22' -- Lagoon Beach/Diep River Mouth
		WHEN site_id = 'ICS09' THEN 'XCS04' -- Sir Lowry's Pass River
		WHEN site_id = 'XCN05' THEN 'CN37' -- Small Bay
	ELSE site_id
	END AS site_id,
	sample_date,
	sample_time,
	analysis_completed,
	sample_temp_c,
	enterococci_cfu_per_100ml AS censored_value,
	CASE
    	WHEN results.enterococci_cfu_per_100ml::text LIKE '%ND%'::text THEN 0::numeric
    	WHEN results.enterococci_cfu_per_100ml::text LIKE '<%'::text THEN regexp_replace(results.enterococci_cfu_per_100ml::text, '<|\s'::text, ''::text)::numeric / 3::numeric
    	WHEN results.enterococci_cfu_per_100ml::text LIKE '>%'::text THEN regexp_replace(results.enterococci_cfu_per_100ml::text, '>|\s'::text, ''::text)::numeric * 3::numeric
    	ELSE regexp_replace(results.enterococci_cfu_per_100ml::text, '\s'::text, ''::text)::numeric
    END AS numeric_value,
	filename,
	lab_code,
	monitoring_group
FROM coastal.results
ORDER BY sample_date DESC, monitoring_group, site_id;

SELECT * FROM coastal.results_view
WHERE site_id = 'CN22';

SELECT * FROM coastal.sites ORDER BY site_description;

GRANT SELECT ON coastal.results_view TO technician;
GRANT SELECT ON coastal.results_view TO anon;

ROLLBACK;
COMMIT;

-- Create a view for the most recent results for each active site

BEGIN;

CREATE VIEW coastal.most_recent_results AS
WITH cte AS (
SELECT
	a.site_description,
	b.site_id,
	b.sample_date,
	b.sample_time,
	b.censored_value,
	b.numeric_value,
	b.lab_code,
	row_number() OVER (PARTITION BY b.site_id ORDER BY b.sample_date DESC) AS row_number
FROM coastal.sites a
	JOIN coastal.results_view b ON a.site_id::text = b.site_id::text
WHERE a.active AND b.monitoring_group = 'routine'
)
SELECT
	cte.site_description,
	cte.site_id,
	cte.sample_date,
	cte.censored_value,
	cte.numeric_value > 240::numeric AS exceeds_240_cfu_per_100ml
FROM cte
WHERE cte.row_number = 1
ORDER BY cte.site_description;

GRANT SELECT ON TABLE coastal.most_recent_results TO anon;
GRANT ALL ON TABLE coastal.most_recent_results TO technician;

SELECT * FROM coastal.most_recent_results;

ROLLBACK;
COMMIT;

-- Create view for the last 8 weeks worth of routine data

-- Create a view for rolling 5-year data category

BEGIN;

CREATE VIEW coastal.rolling5year AS
WITH cte AS (
	SELECT
		b.site_id,
		b.site_description,
		count(*) AS number_of_samples,
		coastal.hazen90(a.numeric_value) AS hazen90,
		coastal.hazen95(a.numeric_value) AS hazen95
	FROM coastal.results_view a
		JOIN coastal.sites b ON a.site_id::text = b.site_id::text
	WHERE a.sample_date > (CURRENT_DATE - '5 years'::interval) AND b.active AND a.monitoring_group = 'routine'
	GROUP BY b.site_id, b.site_description
	ORDER BY b.site_description
)
SELECT
	cte.site_description,
	cte.site_id,
	cte.number_of_samples,
	cte.hazen90,
	cte.hazen95,
	CASE
	WHEN cte.number_of_samples < 10 THEN 'Not enough samples'::text
	WHEN cte.hazen95 <= 100::numeric THEN 'Excellent'::text
	WHEN cte.hazen95 <= 200::numeric THEN 'Good'::text
	WHEN cte.hazen95 > 200::numeric AND cte.hazen90 > 185::numeric THEN 'Poor'::text
	WHEN cte.hazen95 > 200::numeric AND cte.hazen90 < 185::numeric THEN 'Sufficient'::text
	ELSE NULL::text
	END AS water_quality_category
FROM cte;

GRANT SELECT ON TABLE coastal.rolling5year TO anon;
GRANT ALL ON TABLE coastal.rolling5year TO postgres;

ROLLBACK;
COMMIT;

-- Create view for rolling 365 days

BEGIN;

CREATE VIEW coastal.rolling365day AS
WITH cte AS (
	SELECT
		b.site_id,
		b.site_description,
		count(*) AS number_of_samples,
		coastal.hazen90(a.numeric_value) AS hazen90,
		coastal.hazen95(a.numeric_value) AS hazen95
	FROM coastal.results_view a
		JOIN coastal.sites b ON a.site_id::text = b.site_id::text
	WHERE a.sample_date > (CURRENT_DATE - '1 year'::interval) AND b.active AND a.monitoring_group = 'routine'
	GROUP BY b.site_id, b.site_description
	ORDER BY b.site_description
)
SELECT
	cte.site_description,
	cte.site_id,
	cte.number_of_samples,
	cte.hazen90,
	cte.hazen95,
	CASE
	WHEN cte.number_of_samples < 10 THEN 'Not enough samples'::text
	WHEN cte.hazen95 <= 100::numeric THEN 'Excellent'::text
	WHEN cte.hazen95 <= 200::numeric THEN 'Good'::text
	WHEN cte.hazen95 > 200::numeric AND cte.hazen90 > 185::numeric THEN 'Poor'::text
	WHEN cte.hazen95 > 200::numeric AND cte.hazen90 < 185::numeric THEN 'Sufficient'::text
	ELSE NULL::text
	END AS water_quality_category
FROM cte;

SELECT * FROM coastal.rolling365day;
SELECT * FROM coastal.schedule WHERE site_id = 'CS39';
SELECT * FROM coastal.results WHERE site_id = 'CS39' ORDER BY sample_date DESC;

GRANT SELECT ON TABLE coastal.rolling365day TO anon;
GRANT ALL ON TABLE coastal.rolling365day TO postgres;

ROLLBACK;
COMMIT;

-- Update sites view to include active and inactive sites

BEGIN;

DROP VIEW coastal.sites_view;

CREATE OR REPLACE VIEW coastal.sites_view
AS
SELECT
	sites.site_description,
	sites.site_id,
	sites.category,
	st_x(sites.geom) AS long,
	st_y(sites.geom) AS lat,
	active
FROM coastal.sites
ORDER BY site_description;

ALTER TABLE coastal.sites_view
    OWNER TO postgres;

SELECT * FROM coastal.sites_view;

GRANT SELECT ON TABLE coastal.sites_view TO anon;
GRANT ALL ON TABLE coastal.sites_view TO technician;

ROLLBACK;
COMMIT;

-- Create Blue Flag table for last season

BEGIN;

DROP VIEW coastal.blue_flag;

CREATE OR REPLACE VIEW coastal.blue_flag AS
WITH rank_table AS 
(SELECT
	a.site_description,
	a.site_id,
	b.sample_date,
	b.censored_value,
	rank() OVER (PARTITION BY a.site_id, date_trunc('week', b.sample_date) ORDER BY sample_date DESC)
FROM coastal.sites_view a
	JOIN coastal.results_view b ON a.site_id = b.site_id
WHERE b.monitoring_group = 'blue_flag'
ORDER BY sample_date DESC)
SELECT
	site_description,
	site_id,
	date_trunc('week', sample_date)::date AS week,
	censored_value
FROM rank_table
WHERE rank = 1
ORDER BY week DESC, site_description;

SELECT * FROM coastal.blue_flag;

GRANT SELECT ON coastal.blue_flag TO technician;
GRANT SELECT ON coastal.blue_flag TO anon;

ROLLBACK;
COMMIT;

-- Check how many values for each day and site

SELECT
	*,
	count(*) OVER (PARTITION BY site_id, date_trunc('week', sample_date))
FROM coastal.blue_flag
WHERE sample_date > '2024-06-01'
ORDER BY count DESC, sample_date DESC;

-- How to aggregate on first value

BEGIN;

DROP VIEW coastal.blue_flag;

CREATE VIEW coastal.blue_flag AS
WITH cte AS (SELECT
	*,
	rank() OVER (PARTITION BY site_id, date_trunc('week', sample_date) ORDER BY sample_date DESC) AS rank
FROM coastal.blue_flag
WHERE sample_date > '2024-06-01')
SELECT
	site_description,
	site_id,
	week,
	censored_value
FROM cte
WHERE rank = 1;

ROLLBACK;

-- Change all Blue Flag data that are 1's to zeros.

SELECT * FROM coastal.results WHERE monitoring_group = 'blue_flag';

UPDATE coastal.results
SET enterococci_cfu_per_100ml = CASE WHEN enterococci_cfu_per_100ml = '1' THEN 'ND' ELSE enterococci_cfu_per_100ml END
WHERE monitoring_group = 'blue_flag';

SELECT * FROM coastal.results WHERE monitoring_group = 'blue_flag';

ROLLBACK;
COMMIT;

-- Pivot table for routine data

BEGIN;

CREATE VIEW coastal.last_eight_weeks AS
WITH cte AS
(SELECT
	a.site_description,
	a.site_id,
	a.category,
	date_trunc('week', b.sample_date)::date AS week,
	b.censored_value
FROM coastal.sites_view a
	JOIN coastal.results_view b USING (site_id)
WHERE monitoring_group = 'routine' AND a.active
ORDER BY category DESC, site_description)
SELECT
	*
FROM cte
WHERE week >= date_trunc('week', current_date) - '8 weeks'::interval
ORDER BY category DESC, week DESC;

SELECT * FROM coastal.last_eight_weeks;

GRANT SELECT ON coastal.last_eight_weeks TO technician;
GRANT SELECT ON coastal.last_eight_weeks TO anon;

ROLLBACK;
COMMIT;

-- Change all site names to title case

BEGIN;

UPDATE coastal.sites
SET site_description = initcap(site_description);

SELECT site_description, initcap(site_description), initcap(site_description) FROM coastal.sites ORDER BY site_description;

ROLLBACK;
COMMIT;

-- Percentage exceedances over 5 years

BEGIN;

CREATE VIEW coastal.percentage_exceedances_5years AS
WITH counts AS
(SELECT
	site_description,
	site_id,
	count(*)
FROM coastal.sites_view a
	JOIN coastal.results_view b USING (site_id)
WHERE monitoring_group = 'routine'
	AND sample_date > current_date - '5 years'::interval
	AND a.active
GROUP BY site_description, site_id
ORDER BY site_description),
counts_exceedances AS
(SELECT
	site_description,
	site_id,
	count(*)
FROM coastal.sites_view a
	JOIN coastal.results_view b USING (site_id)
WHERE monitoring_group = 'routine'
	AND sample_date > current_date - '5 years'::interval
	AND a.active
	AND b.numeric_value > 240
GROUP BY site_description, site_id
ORDER BY site_description)
SELECT
	a.site_description,
	a.site_id,
	a.count AS total_samples,
	b.count AS total_exceedances,
	round(b.count::numeric / a.count::numeric * 100, 0) AS percentage_exceedances
FROM counts a JOIN counts_exceedances b USING (site_id)
ORDER BY site_description

SELECT * FROM coastal.percentage_exceedances_5years;

ROLLBACK;