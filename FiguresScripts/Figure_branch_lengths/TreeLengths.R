#Tree lengths
# Tree Lengths obtained from RevBayes via script GetTreeLengths.Rev

setwd("manuscript_data/TreeLengths")
comb_relaxed <- read_csv("comb_relaxed_sa.csv")

comb_relaxed <- comb_relaxed %>% 
  mutate(Treatment = "Combined Multirate")


comb_strict <- read_csv("comb_strict.csv")

comb_strict <- comb_strict %>% 
  mutate(Treatment = "Combined  One Rate")

cont_strict <- read_csv("cont_strict_sa.csv")

cont_strict <- cont_strict %>% 
  mutate(Treatment = "Continuous  One Rate")

cont_relaxed <- read_csv("cont_relaxed_sa.csv")

cont_relaxed <- cont_relaxed %>% 
  mutate(Treatment = "Continuous  Multirate")


discrete <- read_csv("discrete_TL.csv")

discrete <- discrete %>% 
  mutate(Treatment = "Discrete")

mega_df <- rbind(comb_strict, comb_relaxed, cont_relaxed, cont_strict, discrete)

mega_df$TL <-as.integer(mega_df$TL)

par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(NULL, xlim=c(100, 450), ylim=c(0,0.055), xlab="Total Tree Length (Ma)", yaxs="i", xaxs="i", main="", ylab="Density")
lines(density(mega_df$TL[mega_df$Treatment=="Combined  One Rate"]), col = adjustcolor("#751F96", alpha.f=0.4), lw=2)
polygon(density(mega_df$TL[mega_df$Treatment=="Combined  One Rate"]), col = adjustcolor("#751F96", alpha.f=0.4), density = 11, angle=-45, border=NA, fillOddEven = T, lw=2.5)
lines(density(mega_df$TL[mega_df$Treatment=="Combined Multirate"]), col = adjustcolor("#751F96", alpha.f=0.4))
polygon(density(mega_df$TL[mega_df$Treatment=="Combined Multirate"]), col = adjustcolor("#751F96", alpha.f=0.4), border=NA)
lines(density(mega_df$TL[mega_df$Treatment=="Continuous  One Rate"]), col = adjustcolor("#FFC107", alpha.f=0.4), lw=2)
polygon(density(mega_df$TL[mega_df$Treatment=="Continuous  One Rate"]), col = adjustcolor("#FFC107", alpha.f=0.4), density = 11, angle=-45, border=NA, fillOddEven = T, lw=2.5)
lines(density(mega_df$TL[mega_df$Treatment=="Continuous  Multirate"]), col = adjustcolor("#FFC107", alpha.f=0.4))
polygon(density(mega_df$TL[mega_df$Treatment=="Continuous  Multirate"]), col = adjustcolor("#FFC107", alpha.f=0.4), border=NA)
lines(density(mega_df$TL[mega_df$Treatment=="Discrete"]), col = adjustcolor("#004D40", alpha.f=0.4), lw=2)
polygon(density(mega_df$TL[mega_df$Treatment=="Discrete"]), col = adjustcolor("#004D40", alpha.f=0.4), density = 11, angle=-45, border=NA, fillOddEven = T, lw=2.5)
legend("topright", inset=c(0.01, 0.01), c("Discrete", "Continuous One Rate", "Continuous Multirate", "Combined One Rate", "Combined Multirate"),
       fill=c("#004D40", "#FFC107", "#FFC107", "#751F96", "#751F96"), density = c(21, 21, NA, 21, NA))