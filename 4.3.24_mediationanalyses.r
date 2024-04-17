install.packages("mediation")
require(mediation)

#mvpa threshold mediation analysis

med.fit_reverse <- glm(formula = comorb_sum ~ pa_mv_convert_levels1 + age_i0 + male + alc_freq_filter + smok_filter + bmi + full_screen_time + employ_status_array_filter_1 + gov_assistance_filter + dep_diag_yes + white_yes + dias_bp_ave + sys_bp_ave, data = data)
summary(med.fit_reverse)

out.fit_reverse <- glm(formula = cevd_event ~  pa_mv_convert_levels1 +comorb_sum+ age_i0 + male + alc_freq_filter + smok_filter + bmi + full_screen_time + employ_status_array_filter_1 + gov_assistance_filter + dep_diag_yes  + white_yes + dias_bp_ave + sys_bp_ave, data = data)
summary(out.fit_reverse)

med.out <- mediate(med.fit_reverse, out.fit_reverse, boot = TRUE, treat = "pa_mv_convert_levels1", mediator = "comorb_sum", sims = 5000)
summary(med.out)

#mvpa continuous mediation analysis

med.fit_reverse <- glm(formula = comorb_sum ~ pa_mv_convert + age_i0 + male + alc_freq_filter + smok_filter + bmi + full_screen_time + employ_status_array_filter_1 + gov_assistance_filter + dep_diag_yes + white_yes + dias_bp_ave + sys_bp_ave, data = data)
summary(med.fit_reverse)

out.fit_reverse <- glm(formula = cevd_event ~  pa_mv_convert +comorb_sum+ age_i0 + male + alc_freq_filter + smok_filter + bmi + full_screen_time + employ_status_array_filter_1 + gov_assistance_filter + dep_diag_yes  + white_yes + dias_bp_ave + sys_bp_ave, data = data)
summary(out.fit_reverse)

med.out <- mediate(med.fit_reverse, out.fit_reverse, boot = TRUE, treat = "pa_mv_convert", mediator = "comorb_sum", sims = 5000)
summary(med.out)