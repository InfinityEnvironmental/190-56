SELECT
	count(*) FILTER (WHERE sample_date > now() - '1 year'::interval AND monitoring_group IN ('daily', 'routine')) AS total_samples_past_year,
	count(*) FILTER (WHERE sample_date > now() - '1 year'::interval AND monitoring_group IN ('daily', 'routine') AND numeric_value < 240) AS total_compliant_samples_past_year,
	(date_trunc('week', now()) - '7 days'::interval)::date AS last_week,
	count(*) FILTER (WHERE date_trunc('week', sample_date) = (date_trunc('week', now()) - '7 days'::interval) AND monitoring_group IN ('routine', 'daily')) AS total_samples_last_week,
	count(*) FILTER (WHERE date_trunc('week', sample_date) = (date_trunc('week', now()) - '7 days'::interval) AND monitoring_group IN ('routine', 'daily') AND site_description ILIKE '%beach%') AS total_samples_at_beaches_last_week,
	count(*) FILTER (WHERE date_trunc('week', sample_date) = (date_trunc('week', now()) - '7 days'::interval) AND monitoring_group IN ('routine', 'daily') AND site_description ILIKE '%beach%' AND numeric_value < 240) AS total_compliant_samples_at_beaches_last_week
FROM coastal.sites a
	JOIN coastal.results_view b ON a.site_id = b.site_id;