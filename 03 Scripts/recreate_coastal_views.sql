-- Recreate all coastal water quality views

-- Sites view

BEGIN;

DROP VIEW coastal.sites_view;

CREATE OR REPLACE VIEW coastal.sites_view
 AS
 SELECT sites.site_description,
    sites.site_id,
    sites.category,
    st_x(sites.geom) AS long,
    st_y(sites.geom) AS lat,
    sites.active
   FROM coastal.sites
  ORDER BY sites.site_description;

GRANT SELECT ON TABLE coastal.sites_view TO anon;
GRANT ALL ON TABLE coastal.sites_view TO postgres;
GRANT ALL ON TABLE coastal.sites_view TO technician;

ROLLBACK;
COMMIT;

-- Results view

BEGIN;

DROP VIEW coastal.results_view;

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
  ORDER BY results.sample_date DESC, results.monitoring_group, (
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
        END);

ALTER TABLE coastal.results_view
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.results_view TO anon;
GRANT ALL ON TABLE coastal.results_view TO postgres;
GRANT SELECT ON TABLE coastal.results_view TO technician;

ROLLBACK;
COMMIT;

-- Blue flag view

BEGIN;

DROP VIEW coastal.blue_flag;

CREATE OR REPLACE VIEW coastal.blue_flag
 AS
 WITH rank_table AS (
         SELECT a.site_description,
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
  ORDER BY (date_trunc('week'::text, rank_table.sample_date::timestamp with time zone)::date) DESC, rank_table.site_description;

ALTER TABLE coastal.blue_flag
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.blue_flag TO anon;
GRANT ALL ON TABLE coastal.blue_flag TO postgres;
GRANT SELECT ON TABLE coastal.blue_flag TO technician;

ROLLBACK;
COMMIT;

-- Last eight weeks view

BEGIN;

DROP VIEW coastal.last_eight_weeks;

CREATE OR REPLACE VIEW coastal.last_eight_weeks
 AS
 WITH cte AS (
         SELECT a.site_description,
            a.site_id,
            a.category,
            date_trunc('week'::text, b.sample_date::timestamp with time zone)::date AS week,
            b.censored_value
           FROM coastal.sites_view a
             JOIN coastal.results_view b USING (site_id)
          WHERE b.monitoring_group = 'routine'::coastal.monitoring_group AND a.active
          ORDER BY a.category DESC, a.site_description
        )
 SELECT cte.site_description,
    cte.site_id,
    cte.category,
    cte.week,
    cte.censored_value
   FROM cte
  WHERE cte.week >= (date_trunc('week'::text, CURRENT_DATE::timestamp with time zone) - '49 days'::interval)
  ORDER BY cte.category DESC, cte.week DESC;

ALTER TABLE coastal.last_eight_weeks
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.last_eight_weeks TO anon;
GRANT ALL ON TABLE coastal.last_eight_weeks TO postgres;
GRANT SELECT ON TABLE coastal.last_eight_weeks TO technician;

ROLLBACK;
COMMIT;

-- Most recent results view

BEGIN;

DROP VIEW coastal.most_recent_results;

CREATE OR REPLACE VIEW coastal.most_recent_results
 AS
 WITH cte AS (
         SELECT a.site_description,
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
        )
 SELECT cte.site_description,
    cte.site_id,
    cte.sample_date,
    cte.censored_value,
    cte.numeric_value > 240::numeric AS exceeds_240_cfu_per_100ml
   FROM cte
  WHERE cte.row_number = 1
  ORDER BY cte.site_description;

ALTER TABLE coastal.most_recent_results
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.most_recent_results TO anon;
GRANT ALL ON TABLE coastal.most_recent_results TO postgres;
GRANT ALL ON TABLE coastal.most_recent_results TO technician;

ROLLBACK;
COMMIT;

-- Percentage exceedances view

BEGIN;

DROP VIEW coastal.percentage_exceedances_5years;

CREATE OR REPLACE VIEW coastal.percentage_exceedances_5years
 AS
 WITH cte AS (
         SELECT a.site_description,
            a.site_id,
            a.category,
            b.numeric_value > 240::numeric AS exceeds_240_cfu_per_100ml
           FROM coastal.sites_view a
             JOIN coastal.results_view b USING (site_id)
          WHERE a.active AND b.monitoring_group = 'routine'::coastal.monitoring_group AND b.sample_date > (CURRENT_DATE - '7 years'::interval)
        )
 SELECT cte.site_description,
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
  GROUP BY cte.category, cte.site_description, cte.site_id
  ORDER BY cte.category DESC, cte.site_description;

ALTER TABLE coastal.percentage_exceedances_5years
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.percentage_exceedances_5years TO anon;
GRANT ALL ON TABLE coastal.percentage_exceedances_5years TO postgres;
GRANT SELECT ON TABLE coastal.percentage_exceedances_5years TO technician;

ROLLBACK;
COMMIT;

-- Rolling 5 year view

BEGIN;

DROP VIEW coastal.rolling5year;

CREATE OR REPLACE VIEW coastal.rolling5year
 AS
 WITH cte AS (
         SELECT b.site_id,
            b.site_description,
            count(*) AS number_of_samples,
            coastal.hazen90(a.numeric_value) AS hazen90,
            coastal.hazen95(a.numeric_value) AS hazen95
           FROM coastal.results_view a
             JOIN coastal.sites b ON a.site_id::text = b.site_id::text
          WHERE a.sample_date > (CURRENT_DATE - '7 years'::interval) AND b.active AND (a.monitoring_group = ANY (ARRAY['routine'::coastal.monitoring_group, 'daily'::coastal.monitoring_group]))
          GROUP BY b.site_id, b.site_description
          ORDER BY b.site_description
        )
 SELECT cte.site_description,
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

ALTER TABLE coastal.rolling5year
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.rolling5year TO anon;
GRANT ALL ON TABLE coastal.rolling5year TO postgres;

ROLLBACK;
COMMIT;

-- Rolling 365 day view

BEGIN;

DROP VIEW coastal.rolling365day;

CREATE OR REPLACE VIEW coastal.rolling365day
 AS
 WITH cte AS (
         SELECT b.site_id,
            b.site_description,
            count(*) AS number_of_samples,
            coastal.hazen90(a.numeric_value) AS hazen90,
            coastal.hazen95(a.numeric_value) AS hazen95
           FROM coastal.results_view a
             JOIN coastal.sites b ON a.site_id::text = b.site_id::text
          WHERE a.sample_date > (CURRENT_DATE - '1 year'::interval) AND b.active AND (a.monitoring_group = ANY (ARRAY['routine'::coastal.monitoring_group, 'daily'::coastal.monitoring_group]))
          GROUP BY b.site_id, b.site_description
          ORDER BY b.site_description
        )
 SELECT cte.site_description,
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

ALTER TABLE coastal.rolling365day
    OWNER TO postgres;

GRANT SELECT ON TABLE coastal.rolling365day TO anon;
GRANT ALL ON TABLE coastal.rolling365day TO postgres;
GRANT ALL ON TABLE coastal.rolling365day TO technician;

ROLLBACK;
COMMIT;
