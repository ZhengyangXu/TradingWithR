# data for AVAV on 8/31/2015
# first value most recent
y      = c(11.42, -8.58, -7.95, 13.21, 21.32, 10.54,
          -0.67, 2.45, -9.77, 13.22, -6.35, 2.63)
y      = y / 100
x      = c(1:12) # 1 is most recent
moves  = data.frame(x, y)
fit    = lm(y ~ x, data=moves)
s_fit  = summary(fit)
rsq    = s_fit$r.squared                     # E15
intrcp = summary(fit)$coefficients[1,1]      # C15 / C17?
slope  = summary(fit)$coefficients[2,1]      # D15

log_y    = log(1+y)                          # col C
log_y2   = log_y^2                           # col D
med_logy = median(log_y)                     # C18
avg_logy = mean(log_y)                       # C19
dv_avg   = (log_y - avg_logy)^2              # col E

rms_pct_change = sqrt(mean(log_y2))          # C21
hist_vol       = rms_pct_change * sqrt(252)  # D21

ror_rmse_lm  = intrcp/rms_pct_change         # D17
ror_rmse_med = med_logy/rms_pct_change       # D18
ror_rmse_avg = avg_logy/rms_pct_change       # D19

p_ror_gtz_l  = pnorm(ror_rmse_lm)            # E17
p_ror_gtz_m  = pnorm(ror_rmse_med)           # E18
p_ror_gtz_a  = pnorm(ror_rmse_avg)           # E19

# print a summary like existing sheet
report = c(intrcp,      med_logy,     avg_logy,
           ror_rmse_lm, ror_rmse_med, ror_rmse_avg,
           p_ror_gtz_l, p_ror_gtz_m,  p_ror_gtz_a)
dim(report) = c(3, 3)
rownames(report) = c("Lin Reg", "Median", "Mean")
colnames(report) = c("LN ROR", "ROR/RMSE", "Pr(ROR>0)")
print(report)
print(sprintf("Historical ann. vol: %1.0f%%", hist_vol*100))