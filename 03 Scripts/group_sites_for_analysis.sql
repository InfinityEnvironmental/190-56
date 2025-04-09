-- Script to create views for coastal water quality dashboard

SELECT * FROM coastal.sites ORDER BY site_id;
BEGIN;

-- Drop the current views

DROP VIEW coastal.most_recent_results;
DROP VIEW coastal.rolling365day;
DROP VIEW coastal.rolling5year;
DROP VIEW coastal.results_view;

-- Create a new results view

SELECT
	CASE
		WHEN site_id = 'XCS16' THEN 'CS41' -- Fish Hoek Beach
		WHEN site_id = 'ICS14' THEN 'CN16O' -- Saunders Rocks
		WHEN site_id = 'ICS12' THEN 'CN11' -- Camps Bay Lifesaving Tower
		WHEN site_id = 'ICS10' THEN 'CN41' -- Camps Bay South
		WHEN site_id = 'ICS15' THEN 'CN05' -- Green Point Pump Station / Mouille Point
		WHEN site_id = 'ICS01' THEN 'CN22' -- Green Point/Mouille Point
		WHEN site_id = 'ICS09' THEN 'XCS04' -- Sir Lowry's Pass River
	ELSE
	END AS site_id,
	sample_date,
	sample_time,
	analysis_completed,
	sample_temp_c,
	enterococci_cfu_per_100ml,
	filename,
	lab_code,
	monitoring_group
FROM coastal.results
WHERE active
ORDER BY sample_date DESC;

CREATE OR REPLACE VIEW coastal.most_recent_results
 AS
 WITH cte AS (
         SELECT a.site_description,
            b.site_id,
            b.sample_date,
            b.sample_time,
            b.analysis_completed,
            b.sample_temp_c,
            b.enterococci_cfu_per_100ml,
            b.filename,
            b.lab_code,
            b.enterococci_cfu_per_100ml_numeric,
            row_number() OVER (PARTITION BY b.site_id ORDER BY b.sample_date DESC) AS row_number
           FROM coastal.sites a
             JOIN coastal.results_view b ON a.site_id::text = b.site_id::text
        )
 SELECT cte.site_description,
    cte.site_id,
    cte.sample_date,
    cte.enterococci_cfu_per_100ml,
    cte.enterococci_cfu_per_100ml_numeric > 240::numeric AS exceeds_240_cfu_per_100ml
   FROM cte
  WHERE cte.row_number = 1
  ORDER BY cte.site_description;

ALTER TABLE coastal.most_recent_results
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.most_recent_results TO anon;
GRANT ALL ON TABLE coastal.most_recent_results TO postgres;

ROLLBACK;

CREATE OR REPLACE VIEW coastal.rolling5year
 AS
 WITH cte AS (
         SELECT b.site_id,
            b.site_description,
            count(*) AS number_of_samples,
            coastal.hazen90(a.enterococci_cfu_per_100ml_numeric) AS hazen90,
            coastal.hazen95(a.enterococci_cfu_per_100ml_numeric) AS hazen95
           FROM coastal.results_view a
             JOIN coastal.sites b ON a.site_id::text = b.site_id::text
          WHERE a.sample_date > (CURRENT_DATE - '5 years'::interval)
          GROUP BY b.site_id, b.site_description
          ORDER BY b.site_description
        )
 SELECT cte.site_id,
    cte.site_description,
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

ALTER TABLE coastal.rolling5year
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.rolling5year TO anon;
GRANT ALL ON TABLE coastal.rolling5year TO postgres;

ROLLBACK;

CREATE OR REPLACE VIEW coastal.rolling365day
 AS
 WITH cte AS (
         SELECT b.site_id,
            b.site_description,
            count(*) AS number_of_samples,
            coastal.hazen90(a.enterococci_cfu_per_100ml_numeric) AS hazen90,
            coastal.hazen95(a.enterococci_cfu_per_100ml_numeric) AS hazen95
           FROM coastal.results_view a
             JOIN coastal.sites b ON a.site_id::text = b.site_id::text
          WHERE a.sample_date > (CURRENT_DATE - '1 year'::interval)
          GROUP BY b.site_id, b.site_description
          ORDER BY b.site_description
        )
 SELECT cte.site_id,
    cte.site_description,
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

ALTER TABLE coastal.rolling365day
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.rolling365day TO anon;
GRANT ALL ON TABLE coastal.rolling365day TO postgres;

ROLLBACK;


