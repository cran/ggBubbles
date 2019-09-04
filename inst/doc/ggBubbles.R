## ----message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE--------
require(knitr)

## ----message=FALSE, warning=FALSE, paged.print=FALSE-----------------------
require(ggplot2)
require(ggBubbles)

## ---- echo = F, fig.wide = TRUE, fig.height=5, fig.width=7.1, fig.cap = "Example of a MiniBubble plot"----
suppressPackageStartupMessages({
    require(dplyr)
    require(tibble)
})
data(MusicianInterestsSmall)
ggplot(data = MusicianInterestsSmall,
       aes(x = Instrument,
           y = Genre,
           col = Level)) +
       geom_point(position = position_surround(), size = 4) +
       scale_colour_manual(values = c("#00e5ff",
                                      "#4694ff",
                                      "#465aff",
                                      "#2c00c9")) + 
       theme_bw(base_size = 18)

## ----message=FALSE, warning=FALSE, paged.print=FALSE-----------------------
require(dplyr)
require(tibble)

## --------------------------------------------------------------------------
data(MusicianInterestsSmall)

## ----eval=FALSE------------------------------------------------------------
#  head(MusicianInterestsSmall)

## ----echo=FALSE------------------------------------------------------------
kable(head(MusicianInterestsSmall),
      caption = "First rows of MusicianInterestsSmall")

## ---- fig.wide = TRUE, fig.height=5, fig.width=7.1, fig.cap = "Traditional Bubble Plot."----
ggplot(data = MusicianInterestsSmall %>% 
               group_by(Instrument, Genre) %>% 
               summarize(Count = n(), AvgLevel = mean(as.integer(Level))),
        aes(x = Instrument, y = Genre, size = Count, col = AvgLevel)) +
        geom_point() + theme_bw(base_size = 18) +
        scale_colour_gradientn(
            colours  = rev(topo.colors(2)),
            na.value = "transparent",
            breaks   = as.integer(MusicianInterestsSmall$Level) %>% 
                             unique %>% sort,
            labels   = levels(MusicianInterestsSmall$Level),
            limits   = c(as.integer(MusicianInterestsSmall$Level) %>% min,
                         as.integer(MusicianInterestsSmall$Level) %>% max)) +
        scale_size_continuous(range = c(3, 11)) 

## ---- fig.wide = TRUE, fig.height=5, fig.width=7.1, fig.cap = "Example of a MiniBubbleplot with position_surround."----
ggplot(data = MusicianInterestsSmall,
       aes(x = Instrument,
           y = Genre,
           col = Level)) +
       geom_point(position = position_surround(), size = 4) +
       scale_colour_manual(values = c("#00e5ff",
                                      "#4694ff",
                                      "#465aff",
                                      "#2c00c9")) + 
       theme_bw(base_size = 18)

## ---- fig.wide = TRUE, fig.height=5, fig.width=7.1, fig.cap = "Example of a MiniBubbleplot with position_surround."----
ggplot(data = MusicianInterestsSmall,
       aes(x = Instrument,
           y = Genre,
           col = Level)) +
       geom_point(position = position_surround(offset = .2), size = 4) +
       scale_colour_manual(values = c("#00e5ff",
                                      "#4694ff",
                                      "#465aff",
                                      "#2c00c9")) + 
       theme_bw(base_size = 18)

## --------------------------------------------------------------------------
data(MusicianInterests)

## ----eval=FALSE------------------------------------------------------------
#  head(MusicianInterests)

## ----echo=FALSE------------------------------------------------------------
kable(head(MusicianInterests),
      caption = "First rows of MusicianInterests")

## ----Musician Genre Interest plot------------------------------------------
p <- ggplot(data        = MusicianInterests,
            aes(x       = Genre,
                y       = Instrument,
                col     = Level)) +
    geom_point(size     = 1.8, 
               position = position_surround(offset = .2))

## ---- fig.cap = "Use case of position_surround. The levels of experiences can be displayed for each individual musician, rather than displaying an average over the group", fig.wide = TRUE, fig.height=7----
p <- p + theme_bw(base_size = 17) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_colour_gradientn(
            colours  = rev(topo.colors(2)),
            na.value = "transparent",
            breaks   = 1:6,
            labels   = c("Interested",
                         "Beginner",
                         "Intermediate",
                         "Experienced",
                         "Very experienced",
                         "Pro")) +
    xlab("") + ylab("")
p

## ---- echo=FALSE, fig.height=7, fig.width=7.1, fig.wide = TRUE, fig.cap = "Demonstration of position_surround algorithm. Points will be added clockwise in layers."----
n <- 25
X <- data.frame(x = rep(1, sum(1:n)),
           y = rep(1, sum(1:n)),
           group = unlist(lapply(1:n, function(x) { rep(x, x) })),
           label =  unlist(lapply(1:n, function(x) { 1:x }))
)
    
ggplot(data = X, 
       aes(x = x, y = y, label = label)) +
       facet_wrap(~group) + 
       geom_text(position = position_surround(offset=.4)) + 
       theme_minimal() + 
       theme( 
           strip.background = element_blank(),
          strip.text.x = element_blank()
       ) + 
       xlim(c(0,2)) + ylim(c(0,2))

