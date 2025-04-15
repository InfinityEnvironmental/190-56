-- Recreate all coastal water quality views
-- Need to drop all views first

-- To do list:
-- Add order number to sites table
-- Add Atlantic or False Bay column to sites table

BEGIN;

-- Drop all views

DROP VIEW coastal.action_required;
DROP VIEW coastal.blue_flag;
DROP VIEW coastal.last_eight_weeks;
DROP VIEW coastal.most_recent_results;
DROP VIEW coastal.percentage_exceedances_5years;
DROP VIEW coastal.rolling365day;
DROP VIEW coastal.rolling5year;
DROP VIEW coastal.results_view;
DROP VIEW coastal.sites_view;

SAVEPOINT drop_views;

-- Update sites table with site order and coastline columns

ALTER TABLE coastal.sites ADD COLUMN site_order integer;

UPDATE coastal.sites a
SET site_order = b.site_order::integer
FROM coastal.site_order b
WHERE a.site_id = b.site_id;

SELECT * FROM coastal.sites ORDER BY site_order;

-- Set daily sites to inactive

UPDATE coastal.sites
SET active = false
WHERE site_id = 'ICS12';

UPDATE coastal.sites
SET active = false
WHERE site_id = 'ICS10';

ROLLBACK TO drop_views;
SAVEPOINT switch_off_sites;

-- Add coastline column

ALTER TABLE coastal.sites ADD COLUMN coastline text;

UPDATE coastal.sites
SET coastline = CASE
	WHEN site_order < 39 THEN 'Atlantic'
	WHEN site_order >= 39 THEN 'False Bay'
	WHEN site_order IS NULL THEN null
	END;

SELECT * FROM coastal.sites ORDER BY site_order;

ROLLBACK TO switch_off_sites;
SAVEPOINT add_coastline;

-- Update GPS points in sites table

UPDATE coastal.sites
SET
  geom = CASE WHEN site_id = 'CN15' THEN st_setsrid(st_makepoint(18.37342417, -33.95924611), 4326)
  	WHEN site_id = 'CN03' THEN st_setsrid(st_makepoint(18.41041667, -33.89903333), 4326)
  	WHEN site_id = 'CN05A' THEN st_setsrid(st_makepoint(18.3979825, -33.90372583), 4326)
  	WHEN site_id = 'CN06A' THEN st_setsrid(st_makepoint(18.39363333, -33.90838333), 4326)
  	WHEN site_id = 'CN12B' THEN st_setsrid(st_makepoint(18.37583194, -33.95507917), 4326)
  	WHEN site_id = 'CN20O' THEN st_setsrid(st_makepoint(18.37458333, -33.94523333), 4326)
  	WHEN site_id = 'CN32' THEN st_setsrid(st_makepoint(18.39706667, -33.90541667), 4326)
  	WHEN site_id = 'CS01' THEN st_setsrid(st_makepoint(18.44993333, -34.12678333), 4326)
  	WHEN site_id = 'CS05' THEN st_setsrid(st_makepoint(18.46766667, -34.11068333), 4326)
  	WHEN site_id = 'CS08' THEN st_setsrid(st_makepoint(18.49395, -34.10061667), 4326)
  	WHEN site_id = 'CS12' THEN st_setsrid(st_makepoint(18.53956667, -34.0903), 4326)
  	WHEN site_id = 'XCS33' THEN st_setsrid(st_makepoint(18.81283333, -34.10166667), 4326)
	 ELSE geom END;

SELECT * FROM coastal.sites;

ROLLBACK TO add_coastline;
SAVEPOINT update_sites;

-- Sites view

CREATE OR REPLACE VIEW coastal.sites_view
AS
SELECT
  sites.site_order,
  sites.site_description,
  sites.site_id,
  sites.category,
  sites.coastline,
  st_y(sites.geom) AS lat,
  st_x(sites.geom) AS long,
  sites.active
FROM coastal.sites
ORDER BY site_order;

ROLLBACK TO update_sites;
SAVEPOINT sites_view;

SELECT * FROM coastal.sites_view;

GRANT SELECT ON TABLE coastal.sites_view TO anon;
GRANT ALL ON TABLE coastal.sites_view TO technician;

-- Results view

CREATE OR REPLACE VIEW coastal.results_view
 AS
 SELECT
        CASE
            WHEN results.site_id::text = 'XCS16'::text THEN 'CS41'::character varying
            WHEN results.site_id::text = 'ICS14'::text THEN 'CN16O'::character varying
            WHEN results.site_id::text = 'ICS12'::text THEN 'CN11'::character varying
            WHEN results.site_id::text = 'ICS10'::text THEN 'CN41'::character varying
            WHEN results.site_id::text = 'ICS15'::text THEN 'CN05'::character varying
            WHEN results.site_id::text = 'ICS01'::text THEN 'CN22'::character varying
            WHEN results.site_id::text = 'ICS09'::text THEN 'XCS04'::character varying
            WHEN results.site_id::text = 'XCN05'::text THEN 'CN37'::character varying
            ELSE results.site_id
        END AS site_id,
    results.sample_date,
    results.sample_time,
    results.analysis_completed,
    results.sample_temp_c,
    results.enterococci_cfu_per_100ml AS censored_value,
        CASE
            WHEN results.enterococci_cfu_per_100ml::text ~~ '%ND%'::text THEN 0::numeric
            WHEN results.enterococci_cfu_per_100ml::text ~~ '<%'::text THEN regexp_replace(results.enterococci_cfu_per_100ml::text, '<|\s'::text, ''::text)::numeric / 3::numeric
            WHEN results.enterococci_cfu_per_100ml::text ~~ '>%'::text THEN regexp_replace(results.enterococci_cfu_per_100ml::text, '>|\s'::text, ''::text)::numeric * 3::numeric
            ELSE regexp_replace(results.enterococci_cfu_per_100ml::text, '\s'::text, ''::text)::numeric
        END AS numeric_value,
    results.filename,
    results.lab_code,
    results.monitoring_group
   FROM coastal.results
  ORDER BY results.sample_date DESC, results.monitoring_group;

SELECT * FROM coastal.results_view;

ROLLBACK TO sites_view;
SAVEPOINT results_view;

GRANT SELECT ON TABLE coastal.results_view TO anon;
GRANT SELECT ON TABLE coastal.results_view TO technician;

-- Blue flag view

CREATE OR REPLACE VIEW coastal.blue_flag
 AS
 WITH rank_table AS (
         SELECT
		 	a.site_order,
		 	a.site_description,
            a.site_id,
            b.sample_date,
            b.censored_value,
            rank() OVER (PARTITION BY a.site_id, (date_trunc('week'::text, b.sample_date::timestamp with time zone)) ORDER BY b.sample_date DESC) AS rank
           FROM coastal.sites_view a
             JOIN coastal.results_view b ON a.site_id::text = b.site_id::text
          WHERE b.monitoring_group = 'blue_flag'::coastal.monitoring_group
          ORDER BY b.sample_date DESC
        )
 SELECT
    rank_table.site_order,
        CASE
            WHEN rank_table.site_id::text = 'CN11'::text THEN 'Camps Bay'::text
            WHEN rank_table.site_id::text = 'CN09'::text THEN 'Clifton Fourth'::text
            WHEN rank_table.site_id::text = 'CS41'::text THEN 'Fish Hoek'::text
            WHEN rank_table.site_id::text = 'XCS08'::text THEN 'Bikini Beach'::text
            WHEN rank_table.site_id::text = 'XCN07'::text THEN 'Melkbosstrand North'::text
            WHEN rank_table.site_id::text = 'XCN15'::text THEN 'Melkbosstrand South'::text
            WHEN rank_table.site_id::text = 'CS34'::text THEN 'Muizenberg'::text
            WHEN rank_table.site_id::text = 'XCN14'::text THEN 'Silwerstroom'::text
            ELSE rank_table.site_description
        END AS site_description,
    rank_table.site_id,
    date_trunc('week'::text, rank_table.sample_date::timestamp with time zone)::date AS week,
        CASE
            WHEN rank_table.censored_value::text = 'ND'::text THEN '0'::character varying
            ELSE rank_table.censored_value
        END AS censored_value
   FROM rank_table
  WHERE rank_table.rank = 1
  ORDER BY (date_trunc('week'::text, rank_table.sample_date::timestamp with time zone)::date) DESC, rank_table.site_order;

SELECT * FROM coastal.blue_flag;

ROLLBACK TO results_view;
SAVEPOINT blue_flag_done;

GRANT SELECT ON TABLE coastal.blue_flag TO anon;
GRANT SELECT ON TABLE coastal.blue_flag TO technician;

-- Last eight weeks view

CREATE OR REPLACE VIEW coastal.last_eight_weeks
 AS
 WITH cte AS (
         SELECT
		 	a.site_order,
		 	a.site_description,
            a.site_id,
            a.category,
            date_trunc('week'::text, b.sample_date::timestamp with time zone)::date AS week,
            b.censored_value,
			b.numeric_value
           FROM coastal.sites_view a
             JOIN coastal.results_view b USING (site_id)
          WHERE b.monitoring_group = 'routine'::coastal.monitoring_group AND a.active
          ORDER BY a.category DESC, a.site_order
        )
 SELECT
 	cte.site_order,
 	cte.site_description,
    cte.site_id,
    cte.category,
    cte.week,
    cte.censored_value,
	cte.numeric_value
   FROM cte
  WHERE cte.week >= (date_trunc('week'::text, CURRENT_DATE::timestamp with time zone) - '49 days'::interval)
  ORDER BY cte.category DESC, cte.site_order, cte.week DESC;

SELECT * FROM coastal.last_eight_weeks;

ROLLBACK TO blue_flag_done;
SAVEPOINT last_eight_weeks_done;

GRANT SELECT ON TABLE coastal.last_eight_weeks TO anon;
GRANT SELECT ON TABLE coastal.last_eight_weeks TO technician;

-- Most recent results view

CREATE OR REPLACE VIEW coastal.most_recent_results
 AS
 WITH cte AS (
         SELECT
		 	a.site_order,
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
          WHERE a.active AND b.monitoring_group = 'routine'::coastal.monitoring_group
		  ORDER BY site_order, sample_date DESC, sample_time DESC
        )
 SELECT
 	cte.site_order,
 	cte.site_description,
    cte.site_id,
    cte.sample_date,
    cte.censored_value,
	cte.numeric_value,
    cte.numeric_value > 240::numeric AS exceeds_240_cfu_per_100ml
   FROM cte
  WHERE cte.row_number = 1
  ORDER BY cte.site_order;

SELECT * FROM coastal.most_recent_results;

ROLLBACK TO last_eight_weeks_done;
SAVEPOINT most_recent_done;

GRANT SELECT ON TABLE coastal.most_recent_results TO anon;
GRANT SELECT ON TABLE coastal.most_recent_results TO technician;

-- Action required view

CREATE OR REPLACE VIEW coastal.action_required
 AS
WITH action_required AS
(WITH cte AS (
         SELECT
		 	a.site_order,
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
          WHERE a.active AND b.monitoring_group = 'routine'::coastal.monitoring_group
		  ORDER BY a.site_order, sample_date DESC, sample_time DESC
		  )
 SELECT
 	cte.site_order,
 	cte.site_description,
    cte.site_id,
	bool_and(numeric_value > 380) AS red
   FROM cte
  WHERE cte.row_number IN (1, 2)
  GROUP BY cte.site_order, cte.site_description, cte.site_id
  ORDER BY cte.site_order)
 SELECT
 	a.site_order,
	a.site_description,
	a.site_id,
	b.sample_date,
	b.censored_value,
	b.numeric_value,
	CASE
		WHEN NOT red AND NOT exceeds_240_cfu_per_100ml THEN 'Green'
		WHEN NOT red AND exceeds_240_cfu_per_100ml THEN 'Amber'
		WHEN red AND exceeds_240_cfu_per_100ml THEN 'Red'
		END AS action_required
FROM action_required a
	JOIN coastal.most_recent_results b ON a.site_id = b.site_id;

SELECT * FROM coastal.action_required;

GRANT SELECT ON TABLE coastal.action_required TO anon;
GRANT SELECT ON TABLE coastal.action_required TO technician;

SAVEPOINT action_required;

-- Percentage exceedances view

CREATE OR REPLACE VIEW coastal.percentage_exceedances_5years
 AS
 WITH cte AS (
         SELECT
		 	a.site_order,
		 	a.site_description,
            a.site_id,
            a.category,
            b.numeric_value > 240::numeric AS exceeds_240_cfu_per_100ml
           FROM coastal.sites_view a
             JOIN coastal.results_view b USING (site_id)
          WHERE a.active AND b.monitoring_group = 'routine'::coastal.monitoring_group AND b.sample_date > (CURRENT_DATE - '7 years'::interval)
        )
 SELECT
 	cte.site_order,
 	cte.site_description,
    cte.site_id,
    cte.category,
    count(*) AS total_samples,
    count(
        CASE
            WHEN cte.exceeds_240_cfu_per_100ml THEN 1
            ELSE NULL::integer
        END) AS total_exceedances,
    round(count(
        CASE
            WHEN cte.exceeds_240_cfu_per_100ml THEN 1
            ELSE NULL::integer
        END)::numeric / count(*)::numeric * 100::numeric, 0) AS percentage_exceedances
   FROM cte
  GROUP BY cte.category, cte.site_order, cte.site_description, cte.site_id
  ORDER BY cte.category DESC, cte.site_order;

SELECT * FROM coastal.percentage_exceedances_5years;

GRANT SELECT ON TABLE coastal.percentage_exceedances_5years TO anon;
GRANT SELECT ON TABLE coastal.percentage_exceedances_5years TO technician;

SAVEPOINT percentage_exceedances_done;

-- Rolling 5 year view

CREATE OR REPLACE VIEW coastal.rolling5year
 AS
 WITH cte AS (
         SELECT
		 	b.site_order,
            b.site_description,
		 	b.site_id,
            count(*) AS number_of_samples,
            coastal.hazen90(a.numeric_value) AS hazen90,
            coastal.hazen95(a.numeric_value) AS hazen95
           FROM coastal.results_view a
             JOIN coastal.sites b ON a.site_id::text = b.site_id::text
          WHERE a.sample_date > (CURRENT_DATE - '7 years'::interval) AND b.active AND (a.monitoring_group = ANY (ARRAY['routine'::coastal.monitoring_group, 'daily'::coastal.monitoring_group]))
          GROUP BY b.site_order, b.site_id, b.site_description
          ORDER BY b.site_order
        )
 SELECT
 	cte.site_order,
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
   FROM cte
   ORDER BY cte.site_order;

SELECT * FROM coastal.rolling5year;

ROLLBACK TO action_required;
SAVEPOINT rolling5year;

GRANT SELECT ON TABLE coastal.rolling5year TO anon;
GRANT SELECT ON TABLE coastal.rolling5year TO technician;

-- Rolling 365 day view

CREATE OR REPLACE VIEW coastal.rolling365day
 AS
 WITH cte AS (
         SELECT
		 	b.site_order,
            b.site_description,
			b.site_id,
            count(*) AS number_of_samples,
            coastal.hazen90(a.numeric_value) AS hazen90,
            coastal.hazen95(a.numeric_value) AS hazen95
           FROM coastal.results_view a
             JOIN coastal.sites b ON a.site_id::text = b.site_id::text
          WHERE a.sample_date > (CURRENT_DATE - '1 year'::interval) AND b.active AND (a.monitoring_group = ANY (ARRAY['routine'::coastal.monitoring_group, 'daily'::coastal.monitoring_group]))
          GROUP BY b.site_order, b.site_id, b.site_description
          ORDER BY b.site_order
        )
 SELECT
 	cte.site_order,
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
SELECT * FROM coastal.results WHERE site_id = 'XCN08' AND monitoring_group IN ('routine', 'daily') AND sample_date > current_date - '1 year'::interval;

GRANT SELECT ON TABLE coastal.rolling365day TO anon;
GRANT SELECT ON TABLE coastal.rolling365day TO technician;

SAVEPOINT rolling365day;

-- Category for summer 2024/2025

CREATE OR REPLACE VIEW coastal.summer_category
 AS
 WITH cte AS (
         SELECT
		 	b.site_order,
            b.site_description,
			b.site_id,
            count(*) AS number_of_samples,
            coastal.hazen90(a.numeric_value) AS hazen90,
            coastal.hazen95(a.numeric_value) AS hazen95
           FROM coastal.results_view a
             JOIN coastal.sites b ON a.site_id::text = b.site_id::text
          WHERE a.sample_date BETWEEN '2024-10-24' AND '2025-03-25' AND b.active AND (a.monitoring_group = ANY (ARRAY['routine'::coastal.monitoring_group, 'daily'::coastal.monitoring_group]))
          GROUP BY b.site_order, b.site_id, b.site_description
          ORDER BY b.site_order
        )
 SELECT
 	cte.site_order,
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

SELECT * FROM coastal.summer_category;
SELECT * FROM coastal.results WHERE site_id = 'XCN08' AND monitoring_group IN ('routine', 'daily') AND sample_date > current_date - '1 year'::interval;

GRANT SELECT ON TABLE coastal.summer_category TO anon;
GRANT SELECT ON TABLE coastal.summer_category TO technician;

SAVEPOINT rolling365day;

ROLLBACK;
COMMIT;
