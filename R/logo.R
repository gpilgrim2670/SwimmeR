install.packages("hexSticker")
library(hexSticker)

s <- sticker(~plot(cars, cex=.5, cex.axis=.5, mgp=c(0,.3,0), xlab="", ylab=""),
             package="hexSticker", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
             filename="inst/extdata/logo.png")
s


sticker("inst/extdata/logo.png", package = "hexSticker", p_size=20, s_x=1, s_y=.75, s_width=.6,
        filename = "inst/extdata/hex_logo.png")

imgurl <- "inst/extdata/logo_nobackground.png"
s <- sticker(
  imgurl,
  package = "SwimmeR",
  p_size = 15,
  p_color = "black",
  p_family = "gochi",
  s_x = 1,
  s_y = 0.75,
  s_width = 0.8,
  h_fill="white",
  h_color="black",
  filename = "inst/extdata/hex_logo.png")

plot(s)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Gochi Hand", "gochi")
## Automatically use showtext to render text for future devices
showtext_auto()
