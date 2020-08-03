

require(ggplot2)
data(mpg)


plot.new()
par(mar = c(4,4,4,1))
x_range = c(1.4, 7)
y_range = c(9, 45)
d_x = diff(x_range)
d_y = diff(y_range)
plot.window(xlim = x_range + c(0, 1.5),
            ylim = y_range + c(-1, 1))
groups = unique(mpg$class)
sapply(seq(y_range[1], y_range[2], length = 11), function(x) lines(x_range, c(x, x), col = 'grey75', lwd = .3))
sapply(seq(y_range[1], y_range[2], length = 6), function(x) lines(x_range, c(x, x), col = 'grey75', lwd = .6))
sapply(seq(x_range[1], x_range[2], length = 11), function(x) lines(c(x, x), y_range, col = 'grey75', lwd = .3))
sapply(seq(x_range[1], x_range[2], length = 6), function(x) lines(c(x, x), y_range, col = 'grey75', lwd = .6))
x_rect = c(x_range + d_x * c(-.02,.02))
y_rect = c(y_range + d_y * c(-.02,.02))
rect(x_rect[1], y_rect[1], x_rect[2], y_rect[2])
base_cols = colorRampPalette(yarrr::piratepal('southpark'))(length(groups))
cols = sapply(mpg$class, function(x) base_cols[which(x == groups)])
points(mpg$displ, mpg$hwy, pch = 16, col = cols, cex = 1.2)
ys = y_range[1] + d_y * seq(.2, .6, length = length(groups))
text(x_range[2] + .7, y = y_range[1] + d_y * .7, label = 'class')
text(x = rep(x_range[2] + .8, length(groups)),
     y = ys, label = groups, adj = 0, cex = .8)
points(rep(x_range[2] + .6, length(groups)), ys, col = base_cols, pch = 16, cex = 1.2)
mtext(c('Engine Displacement in Liters', 'Highway miles per Gallon'),
      side = c(1, 2), line = c(1, 1.5),
      at = c(x_range[1] + d_x * .5, y_range[1] + d_y * .5))
mtext('MPG data', side = 3, at = 1.3, adj = 0, line = .5)
mtext('Cars with higher engine displacement tend to have lower highway mpg',
      side = 3, at = 1.3, adj = 0, line = -.5, cex = .75)
x_ax = c(10, 20, 30, 40)
y_ax = 2:7
mtext(x_ax, side = 2, at = x_ax, cex = .8, line = -.3, las = 1)
mtext(y_ax, side = 1, at = y_ax, cex = .8, line = -.5)
sapply(x_ax, function(x) lines(c(x_rect[1], x_rect[1] - d_x * .01),c(x, x)))
sapply(y_ax, function(x) lines(c(x, x),c(y_rect[1], y_rect[1] - d_y * .015)))
xs = mpg$displ
plx = predict(lm(mpg$hwy ~ xs), se = TRUE)
fs = plx$fit
lines(xs, fs, col = 'blue', lwd = 3)
px = c(xs, rev(xs))
py1 = smooth.spline(xs, fs + qt(0.975, plx$df) * plx$se)
py2 = smooth.spline(xs, fs - qt(0.975, plx$df) * plx$se)
polygon(c(py1$x, rev(py2$x)), c(py1$y, rev(py2$y)),
        col = rgb(.5,.5,.5, alpha = .3), border = NA)
