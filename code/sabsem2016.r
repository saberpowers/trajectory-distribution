# Make sure you have all of the below packages installed before running script
library(colorspace)
library(dplyr)
library(ggplot2)
library(glmnet)
library(graphics)
library(hexbin)
library(Hmisc)
library(Matrix)
library(ranger)

source('code/util.r')

data = read.csv('data/savant_data.csv')
data = data %>% filter(!is.na(hit_speed) & hit_speed > 30 & hit_speed < 120)

players = read.csv('data/players.csv')
player = as.character(players$mlb_name)
names(player) = players$mlb_id
player['0'] = 'Replacement Level'

n.bat = aggregate(data$batter, list(batter = data$batter), length)
replacement.batters = n.bat$batter[n.bat$x < 70]
n.pit = aggregate(data$pitcher, list(pitcher = data$pitcher), length)
replacement.pitchers = n.pit$pitcher[n.pit$x < 70]
data$batter[is.element(data$batter, replacement.batters)] = 0
data$pitcher[is.element(data$pitcher, replacement.pitchers)] = 0

w = rep(0, length(unique(data$events)))
names(w) = sort(unique(data$events))
w['Single'] = .878
w['Double'] = 1.243
w['Triple'] = 1.572
w['Home Run'] = 2.021
woba = w[as.character(data$events)]

woba.model = ranger(woba ~ hit_speed + hit_angle, data, write.forest = TRUE,
  mtry = 1, min.node.size = 500)
ewoba = woba.model$predictions

x = data$hit_speed*cos(rad(data$hit_angle))
y = data$hit_speed*sin(rad(data$hit_angle))

## For figs/woba.png:

#smoothScatter(y ~ x, nbin=1000,
#    colramp = colorRampPalette(c('darkblue', 'blue', 'lightblue', 'green',
#    'yellow', 'orange', 'darkorange', 'red', 'darkred')), nrpoints = 0,
#    transformation = function(x) x^.75, axes = FALSE, xlab = '', ylab = '',
#    ylim = c(-45, 75))
#abline(0, rad(10), col = 'white', lwd = 2)
#abline(0, rad(25), col = 'white', lwd = 2)
#abline(0, rad(50), col = 'white', lwd = 2)
#text(117, 15, expression(10*degree), col = 'white', cex = 2)
#text(117, 44, expression(25*degree), col = 'white', cex = 2)
#text(96, 75, expression(50*degree), col = 'white', cex = 2)
#xcircle = cos(rad(seq(-90, 90, length = 1000)))
#ycircle = sin(rad(seq(-90, 90, length = 1000)))
#lines(70*xcircle, 70*ycircle, lty = 2, col = 'white', lwd = 2)
#lines(100*xcircle, 100*ycircle, lty = 2, col = 'white', lwd = 2)
#text(68, -45, '70mph', col = 'white', cex = 2)
#text(105, -45, '100mph', col = 'white', cex = 2)

## For figs/density.png:

#hbin = hexbin(x, y, xbins = 100, IDs = TRUE)
#mtrans = hexTapply(hbin, woba, mean, na.rm = TRUE)
#pushHexport(hexViewport(hbin, ybnds = c(-50, 100)))
#grid.hexagons(hbin, use.count = FALSE, cell.at = mtrans, mincnt = min(mtrans),
#  maxcnt = max(mtrans), colramp = colorRampPalette(pal(50)))
#llines(200*c(0, 1), 200*c(0, 2*pi*10/360), col = 'black', lwd = 2)
#llines(200*c(0, 1), 200*c(0, 2*pi*25/360), col = 'black', lwd = 2)
#llines(200*c(0, 1), 200*c(0, 2*pi*50/360), col = 'black', lwd = 2)
#ltext(117, 15, expression(10*degree), cex = 2)
#ltext(117, 44, expression(25*degree), cex = 2)
#ltext(96, 75, expression(50*degree), cex = 2)
#llines(70*xcircle, 70*ycircle, col = 'black', lty = 2, lwd = 2)
#llines(100*xcircle, 100*ycircle, col = 'black', lty = 2, lwd = 2)
#ltext(68, -45, '70mph', cex = 2)
#ltext(105, -45, '100mph', cex = 2)

################################
# Part I: Modelling launch angle
################################

grid.angle = seq(-100, 100)
set.seed(1623)
plotting.subset = sample(1:nrow(data), 2000)

pdf('figs/hist-angle.pdf', width = 5, height = 5)
hist(data$hit_angle, breaks = 100, freq = FALSE, main = '',
  xlab = 'Launch angle', axes = FALSE,
  xlim = quantile(data$hit_angle, c(.001, .999)))
lines(grid.angle, dnorm(grid.angle, mean(data$hit_angle), sd(data$hit_angle)),
  col = 'dodgerblue', lwd = 4)
hist(data$hit_angle, breaks = 100, freq = FALSE, add = TRUE)
axis(1, at = c(-45, 10, 25, 50))
dev.off()

pdf('figs/qq-angle.pdf', width = 5, height = 5)
qqnorm(data$hit_angle[plotting.subset], axes = FALSE,
  main = '', xlab = 'Normal quantiles')
abline(mean(data$hit_angle), sd(data$hit_angle), lwd = 4, col = 'dodgerblue')
axis(1)
axis(2)
dev.off()

batter = player[as.character(data$batter)]
pitcher = player[as.character(data$pitcher)]
oppo = data$stand != data$p_throws
home = data$inning_topbot == 'bot'
tag = c(rep('oppo0', sum(!oppo)), rep('oppo1', sum(oppo)),
  rep('home0', sum(!home)), rep('home1', sum(home)),
  paste('s', data$home_team, sep = ''), paste('b', data$batter, sep = ''),
  paste('p', data$pitcher, sep = ''))
X = sparseMatrix(c(which(!oppo), which(oppo), which(!home), which(home),
  rep(1:nrow(data), 3)), as.numeric(as.factor(tag)))

tag.name = c(rep('oppo0', sum(!oppo)), rep('oppo1', sum(oppo)),
  rep('home0', sum(!home)), rep('home1', sum(home)),
  paste('s', data$home_team, sep = ''), paste('b', batter), paste('p', pitcher))
names(tag.name) = tag
label = tag.name[sort(unique(tag))]

ols.angle = glmnet(X, data$hit_angle, alpha = 0, lambda = .Machine$double.xmin,
  standardize = FALSE)
residual = data$hit_angle - predict(ols.angle, X)[, 1]

ridge.var = cv.glmnet(X, residual^2, alpha = 0, standardize = FALSE)
var.angle = predict(ridge.var, X, s = 'lambda.min')
coef.var = coef(ridge.var)[, 1]
names(coef.var) = c('Intercept', label)
pred.var = predict(ridge.var, diag(ncol(X)), s = 'lambda.min')[, 1]
names(pred.var) = label

ridge.angle = cv.glmnet(X, data$hit_angle, alpha = 0, weights = 1/var.angle,
  standardize = FALSE, lambda.min.ratio = 1e-8)
coef.angle = coef(ridge.angle, s = 'lambda.min')[, 1]
names(coef.angle) = c('Intercept', label)
pred.angle = predict(ridge.angle, diag(ncol(X)), s = 'lambda.min')[, 1]
names(pred.angle) = label
pred.angle = pred.angle[c(grep('^b', names(pred.angle)),
  grep('^p', names(pred.angle)))]

n = aggregate(data$hit_angle, by = list(batter = data$batter), length)$x
mean.angle = aggregate(data$hit_angle, by = list(batter = data$batter), mean)$x
sd.angle = aggregate(data$hit_angle, by = list(batter = data$batter), sd)$x

head(-sort(-pred.angle))
head(sort(pred.angle))

pdf('figs/angle-distribution.pdf', width = 5, height = 5)
plot(density(mean.angle), ylim = c(0, .15), col = 'dodgerblue',
  lwd = 2, axes = FALSE, main = '', xlab = 'Launch angle mean')
lines(density(pred.angle), col = 'dodgerblue', lty = 2, lwd = 2)
axis(1, at = c(0, 10, 25, 45))
axis(2)
legend('topright', c('Observed', 'Estimated'), lty = 1:2, lwd = 2,
  col = 'dodgerblue', bty = 'n')
dev.off()

head(sort(sqrt(pred.var)))
head(-sort(-sqrt(pred.var)))

pdf('figs/var-distribution.pdf', width = 5, height = 5)
plot(density(sd.angle), ylim = c(0, .6), col = 'dodgerblue',
  lwd = 2, axes = FALSE, main = '', xlab = 'Launch angle s.d.')
lines(density(sqrt(pred.var)), col = 'dodgerblue', lty = 2, lwd = 2)
axis(1, at = c(20, 22, 24, 26, 30))
axis(2)
legend('topright', c('Observed', 'Estimated'), lty = 1:2, lwd = 2,
  col = 'dodgerblue', bty = 'n')
dev.off()

pdf('figs/angle-diagnostic.pdf', height = 5)
fitted.angle = predict(ridge.angle, X, s = 'lambda.min')[, 1]
r = data$hit_angle - fitted.angle
r.mean = aggregate(r, list(pred = round(fitted.angle)), mean)
r.sd = aggregate(r, list(pred = round(fitted.angle)), sd)
r.n = aggregate(r, list(pred = round(fitted.angle)), length)
errbar(r.mean$pred, r.mean$x, r.mean$x - 3 * r.sd$x / sqrt(r.n$x),
  r.mean$x + 3 * r.sd$x / sqrt(r.n$x), col = 'dodgerblue',
  errbar.col = 'dodgerblue', axes = FALSE, xlab = 'Predicted launch angle',
  ylab = 'Residual')
abline(h = 0, lty = 2)
axis(1, at = c(0, 10, 25))
axis(2)
dev.off()

pdf('figs/angle-diagnostic-zoomed.pdf', height = 5)
fitted.angle = predict(ridge.angle, X, s = 'lambda.min')[, 1]
r = data$hit_angle - fitted.angle
r.mean = aggregate(r, list(pred = round(fitted.angle)), mean)
r.sd = aggregate(r, list(pred = round(fitted.angle)), sd)
r.n = aggregate(r, list(pred = round(fitted.angle)), length)
errbar(r.mean$pred, r.mean$x, r.mean$x - 3 * r.sd$x / sqrt(r.n$x),
  r.mean$x + 3 * r.sd$x / sqrt(r.n$x), col = 'dodgerblue',
  errbar.col = 'dodgerblue', axes = FALSE, xlab = 'Predicted launch angle',
  ylab = 'Residual', ylim = c(-20, 20))
abline(h = 0, lty = 2)
axis(1, at = c(0, 10, 25))
axis(2)
dev.off()

##############################################################
# Part II: Modelling exit velocity conditional on launch angle
##############################################################

grid.speed = seq(30, 120)
grid.trans = log(130 - seq(30, 120))

pdf('figs/hist-speed.pdf', height = 5, width = 5)
hist(data$hit_speed, breaks = 100, main = '', xlab = 'Exit velocity',
  freq = FALSE, axes = FALSE)
axis(1)
dev.off()

pdf('figs/qq-speed.pdf', height = 5, width = 5)
qqnorm(data$hit_speed[plotting.subset], main = '', axes = FALSE)
abline(mean(data$hit_speed), sd(data$hit_speed), col = 'dodgerblue', lwd = 4)
axis(1)
axis(2)
dev.off()

trans = log(130 - data$hit_speed)

pdf('figs/hist-trans.pdf', height = 5, width = 5)
hist(trans, breaks = 100, main = '', freq = FALSE, axes = FALSE,
  xlab = 'log(130 - exit velocity)', ylim = c(0, 1.2))
lines(grid.trans, dnorm(grid.trans, mean(trans), sd(trans)),
  col = 'dodgerblue', lwd = 4)
hist(trans, breaks = 100, freq = FALSE, add = TRUE)
axis(1)
dev.off()

pdf('figs/qq-trans.pdf', height = 5, width = 5)
qqnorm(trans[plotting.subset], main = '', axes = FALSE)
abline(mean(trans), sd(trans), col = 'dodgerblue', lwd = 4)
axis(1)
axis(2)
dev.off()

t.mean = aggregate(trans, list(angle = round(data$hit_angle)), mean)
t.sd = aggregate(trans, list(angle = round(data$hit_angle)), sd)
t.n = aggregate(trans, list(angle = round(data$hit_angle)), length)

pdf('figs/trans-v-angle.pdf', height = 5)
errbar(t.mean$angle, t.mean$x, t.mean$x - 3 * t.sd$x / sqrt(t.n$x),
  t.mean$x + 3 * t.sd$x / sqrt(t.n$x), col = 'dodgerblue',
  errbar.col = 'dodgerblue', axes = FALSE, xlab = 'Launch angle',
  ylab = 'log(130 - exit velocity)')
axis(1, at = c(-45, 10, 25, 50))
axis(2)
dev.off()

pdf('figs/trans-v-angle-curve.pdf', height = 5)
errbar(t.mean$angle, t.mean$x, t.mean$x - 3 * t.sd$x / sqrt(t.n$x),
  t.mean$x + 3 * t.sd$x / sqrt(t.n$x), col = 'dodgerblue',
  errbar.col = 'dodgerblue', axes = FALSE, xlab = 'Launch angle',
  ylab = 'log(130 - exit velocity)')
axis(1, at = c(-45, 10, 25, 50))
axis(2)
lines(grid.angle, 5.21 - 1.65 * cos(rad(grid.angle - 10)), lwd = 4)
dev.off()

path = exp(seq(0, -10, length = 100))
Xbig = cbind(cos(rad(data$hit_angle - 10)), X)
ridge.trans = cv.glmnet(Xbig, trans, alpha = 0, standardize = FALSE,
  lambda = path)
coef.trans = coef(ridge.trans, s = 'lambda.min')[, 1]
names(coef.trans) = c('Intercept', 'angle', label)
var.trans = sum((trans - predict(ridge.trans, Xbig, s = 'lambda.min')[, 1])^2)/
  (nrow(Xbig) - ncol(Xbig))

design = diag(ncol(Xbig))
design[, 1] = cos(rad(10))
pred.trans = predict(ridge.trans, design, s = 'lambda.min')[, 1]
names(pred.trans) = c('angle', label)
pred.speed = 130 - exp(pred.trans)
pred.speed = pred.speed[c(grep('^b', names(pred.speed)),
  grep('^p', names(pred.speed)))]

mean.speed = aggregate(data$hit_speed, by = list(batter = data$batter), mean)$x
var.speed= aggregate(data$hit_speed, by = list(batter = data$batter), var)$x

head(-sort(-pred.speed))
head(sort(pred.speed))

pdf('figs/speed-distribution.pdf', width = 5, height = 5)
plot(density(mean.speed), xlim = c(78, 98), ylim = c(0, .3),
  col = 'dodgerblue',
  lwd = 2, axes = FALSE, main = '', xlab = 'Exit velocity')
lines(density(pred.speed), col = 'dodgerblue', lty = 2, lwd = 2)
axis(1, at = c(85, 91, 95))
axis(2)
legend('topleft', c('Observed', 'Estimated (a = 10)'), lty = 1:2, lwd = 2,
  col = 'dodgerblue', bty = 'n')
dev.off()

###################################
# Part III: Putting it all together
###################################

mesh.angle = seq(-90, 90, length = 1000)
mesh.speed = seq(60, 120, length = 1000)

angle = matrix(mesh.angle, nrow = length(mesh.angle),
  ncol = length(mesh.speed))
speed = matrix(mesh.speed, nrow = length(mesh.angle),
  ncol = length(mesh.speed), byrow = TRUE)
distribution = data.frame(angle = c(angle), speed = c(speed))
distribution$hit_angle = distribution$angle
distribution$hit_speed = distribution$speed
distribution$EwOBAcon = predict(woba.model, distribution)$prediction

circle70 = (70*data.frame(x=cos(rad(mesh.angle)), y=sin(rad(mesh.angle)))) %>%
  filter(x > 0 & x < 120) %>% filter(y > -60 & y < 80)
circle100 = (100*data.frame(x=cos(rad(mesh.angle)), y=sin(rad(mesh.angle)))) %>%
  filter(x > 0 & x < 120) %>% filter(y > -60 & y < 80)


matchup.heatmap = function(b, p) {
  mean.angle = sum(coef.angle[c('Intercept', paste('b', b), paste('p', p))])
  var.angle = sum(coef.var[c('Intercept', paste('b', b), paste('p', p))])
  mean.trans = sum(coef.trans[c('Intercept', paste('b', b), paste('p', p))]) +
    coef.trans['angle'] * cos(rad(mesh.angle - 10))
  mean.trans.expanded = matrix(mean.trans, nrow = length(mesh.angle),
    ncol = length(mesh.speed))
  density = dnorm(angle, mean.angle, sqrt(var.angle)) *
    dnorm(log(130 - speed), mean.trans.expanded, sqrt(var.trans))/(130 - speed)
  density = density/sum(density)
  distribution$density = c(density)
  distribution$x = distribution$speed*cos(rad(distribution$angle))
  distribution$y = distribution$speed*sin(rad(distribution$angle))
  distribution =
    distribution %>% filter(x > 0 & x < 120) %>% filter(y > -60 & y < 80)
  EwOBAcon = sum(distribution$density * distribution$EwOBAcon)

  ggplot(distribution, aes(x, y, z = density)) +
    stat_summary_hex(bins = 100) + theme_classic() +
    scale_fill_gradientn(colors = rev(rainbow(9, end = 2/3))) +
    theme(legend.position = 'none', plot.title = element_text(size = 24)) +
    geom_abline(intercept = 0, slope = tan(rad(10)), size = 1.5) +
    geom_abline(intercept = 0, slope = tan(rad(25)), size = 1.5) +
    geom_abline(intercept = 0, slope = tan(rad(45)), size = 1.5) +
    geom_path(aes(x,y),circle70,inherit.aes=FALSE,linetype='dashed',size=1.5) +
    geom_path(aes(x,y),circle100,inherit.aes=FALSE,linetype='dashed',size=1.5)+
    annotate('text', 60, -55, label = '70mph', size = 8) +
    annotate('text', 100, -55, label = '100mph', size = 8) +
    annotate('text', 125, 15, label = 10, size = 8) +
    annotate('text', 117, 49, label = 25, size = 8) +
    annotate('text', 85, 77, label = 50, size = 8) +
    annotate('text', 85, 77, label = 50, size = 8) +
    annotate('text', 25, -25, label =
      paste('EwOBAcon =', substring(sprintf('%.3f', EwOBAcon), 2)), size = 8) +
    ggtitle(paste(b, 'v.', p)) + no.axes
}


pdf('figs/arenado-young.pdf')
matchup.heatmap('Nolan Arenado', 'Chris Young')
dev.off()

pdf('figs/yelich-stroman.pdf')
matchup.heatmap('Christian Yelich', 'Marcus Stroman')
dev.off()

pdf('figs/gordon-sabathia.pdf')
matchup.heatmap('Dee Gordon', 'CC Sabathia')
dev.off()

pdf('figs/stanton-nova.pdf')
matchup.heatmap('Giancarlo Stanton', 'Ivan Nova')
dev.off()

