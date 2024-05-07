library(tidyverse)
library(lme4)
library(lmerTest)
library(data.table)
library(interactions)
library(patchwork)

N_iterations = 5000

### DATA PREPARATION ###

done_and_crashed <- read.csv("experimental_eye_data_fixations_experiment2.csv")

#renaming
done_and_crashed<- done_and_crashed %>%
  group_by(ID) %>%
  mutate(block = case_when(drift == "nodrift" ~ 'normal',
                           drift == "fake" ~ 'fake',
                           drift == "invisible" ~ 'invisible'))


#getting rid of the first trial in every block 
done_and_crashed <- done_and_crashed %>%
  group_by(ID) %>%
  arrange(trial, .by_group = TRUE) %>%
  mutate(prevBlock = shift(block, n=1L, fill=NA, type=c("lag"), give.names=FALSE))

done_and_crashed<- done_and_crashed %>%
  group_by(ID) %>%
  mutate(transition = case_when(block != prevBlock  ~ 'yes',
                           .default = "no"))

done_and_crashed_transitions <- done_and_crashed %>%
  filter(transition == "yes")

done_and_crashed_transitions <- dplyr::select(done_and_crashed_transitions, c(ID, trial, transition))

done_and_crashed <- dplyr::select(done_and_crashed, -c(transition))

df <- merge(done_and_crashed, done_and_crashed_transitions, by=c("ID","trial"), all = TRUE)

df<- df %>%
  mutate(transition = case_when(transition == 'yes'  ~ 'yes',
                                .default = "no"))

df<- df %>%
  filter(transition != 'yes')

df<- df %>%  
  group_by(ID) %>%
  filter(trial != min(trial))

## releveling and adjustment of the variables
df$distance_to_spaceship.log <- log(df$distance_to_spaceship)

df <- df %>%
  mutate(block = relevel(factor(block),  ref = "normal"))

## splitiing data into distand (exploring) and close (resting) fixations
exploring_df <- df[df$distance_to_spaceship > 5, ]
resting_df  <- df[df$distance_to_spaceship < 5, ]


#### DISTANT FIXATIONS
## Models

dist2ship_explr <-  lm(distance_to_spaceship.log ~ N_visible_drift_tiles  * block + N_visible_obstacles 
                           , data=exploring_df)
dist2ship_explr_id <- lmer(distance_to_spaceship.log ~ N_visible_drift_tiles  * block + N_visible_obstacles 
                          + (1|ID), data=exploring_df, REML = FALSE)
dist2ship_explr_full <- lmer(distance_to_spaceship.log ~ N_visible_drift_tiles  * block + N_visible_obstacles 
                             + (1+block|ID), data=exploring_df, REML = FALSE)
## Comparisson
BIC(dist2ship_explr, 
    dist2ship_explr_id, 
    dist2ship_explr_full) #WINNER

# Summary for a winner
summary(dist2ship_explr_full)

# Computation of the confidence intervals (Long time -> N_iteration = 5000)
fix_dist_int <- confint(dist2ship_explr_full, nsim=N_iterations, parm=c(), method="boot")

#Plots 
Dist_explr_obj <- interact_plot(dist2ship_explr_full, pred = N_visible_obstacles, modx = block,
                                   #mod2 = N_visible_drift_tiles,
                                   vary.lty = FALSE,
                                   #interval = TRUE,
                                   mod2.labels = c("No Drift Tiles",  "1 Drift Tiles", "2 Drift Tiles"),
                                   x.label = "Number of Visible obstacles",
                                   y.label = "Distnace to Ship° (log)",
                                   legend.main = "Block",
                                modx.labels = c("Normal", "Fake", "Invisible"))

Dist_explr_drift <- interact_plot(dist2ship_explr_full, pred = N_visible_drift_tiles, modx = block,
                                   #mod2 = N_visible_drift_tiles,
                                   vary.lty = FALSE,
                                   #interval = TRUE,
                                   mod2.labels = c("No Drift Tiles",  "1 Drift Tiles", "2 Drift Tiles"),
                                   x.label = "Number of Drifts",
                                   y.label = "Distnace to Ship° (log)",
                                   legend.main = "Number of drift tiles",
                                  main.title = "Distant Fixations") #main.title = "Distant Fixation Regions")


#### CLOSE FIXATIONS
## Models

dist2ship_rest <-  lm(distance_to_spaceship.log ~  N_visible_drift_tiles  * block + N_visible_obstacles
                       , data=resting_df)
dist2ship_rest_id <- lmer(distance_to_spaceship.log ~  N_visible_drift_tiles  * block + N_visible_obstacles
                           + (1|ID), data=resting_df, REML = FALSE)
dist2ship_rest_full <- lmer(distance_to_spaceship.log ~ N_visible_drift_tiles  * block + N_visible_obstacles
                             + (1+block|ID), data=resting_df, REML = FALSE)

## Comparisson
BIC(dist2ship_rest, 
    dist2ship_rest_id,
    dist2ship_rest_full)

# Summary for a winner
summary(dist2ship_rest_full)

# Computation of the confidence intervals (Long time -> N_iteration = 5000)
fix_close_int <- confint(dist2ship_rest_full, nsim=N_iterations, parm=c(), method="boot")

# Plots
Dist_rest_obj <- interact_plot(dist2ship_rest_full, pred = N_visible_obstacles, modx = block,
                                #mod2 = N_visible_drift_tiles,
                                vary.lty = TRUE,
                                #interval = TRUE,
                                mod2.labels = c("No Drift Tiles",  "1 Drift Tiles", "2 Drift Tiles"),
                                x.label = "Number of Visible obstacles",
                                y.label = "Distnace to Ship° (log)",
                                legend.main = "Block") 

Dist_rest_obj <- Dist_rest_obj + scale_linetype_manual(
    name = "Block",
    values = c("solid", "dashed", "solid")) #adjusting lines since to lines at exactly same place
                            

Dist_rest_drift <- interact_plot(dist2ship_rest_full, pred = N_visible_drift_tiles, modx = block,
                             #mod2 = N_visible_drift_tiles,
                             vary.lty = FALSE,
                             #interval = TRUE,
                             mod2.labels = c("No Drift Tiles",  "1 Drift Tiles", "2 Drift Tiles"),
                             x.label = "Block",
                             y.label = "Distnace to Ship° (log)",
                             legend.main = "Number of drift tiles",
                             main.title = "Close Fixations")



###COMBINING PLOTS

rest <- Dist_rest_drift + theme(legend.position = "none", axis.title=element_blank()) + ylim(0.75, 0.95) + 
  Dist_rest_obj + theme(legend.position = "none", axis.title=element_blank()) + ylim(0.75, 0.95)

explr  <- Dist_explr_drift + theme(legend.position = "none", axis.title.y=element_blank()) + ylim(1.85, 2.15)  + 
  Dist_explr_obj + theme(axis.title.y=element_blank()) + ylim(1.85, 2.15) 


all <- (rest + 
          plot_annotation('Close Fixations', theme=theme(plot.title=element_text( face = "bold", hjust=0.5)))) / 
  (explr + 
     plot_annotation('Distant Fixations', theme=theme(plot.title=element_text( face = "bold", hjust=0.5)))) + plot_layout(guides = "collect")

  
wrap_elements(all) +
  labs(tag = "Distnace to Ship° (log)") +
  theme(
    plot.tag = element_text(size = rel(1), angle = 90, face = "bold"),
    plot.tag.position = "left"
  )
  

### Learning effect

done_and_crashed <- read.csv("experimental_eye_data_fixations_experiment2.csv")

## Adjusting data to exclude normalisation trials between blocks
done_and_crashed_norm <- done_and_crashed %>%
  filter(drift == "nodrift" & trial < 21) %>%
  group_by(ID) %>%
  mutate(trial_block = trial - min(trial) + 1) %>%
  mutate(trial_percentage = (trial_block/(max(trial) - min(trial) + 1))*100)

done_and_crashed_fake <- done_and_crashed %>%
  filter(drift == "fake") %>%
  group_by(ID) %>%
  mutate(trial_block = trial - min(trial) + 1) %>%
  mutate(trial_percentage = (trial_block/(max(trial) - min(trial) + 1))*100)

done_and_crashed_invisible <- done_and_crashed %>%
  filter(drift == "invisible") %>%
  group_by(ID) %>%
  mutate(trial_block = trial - min(trial) + 1) %>%
  mutate(trial_percentage = (trial_block/(max(trial) - min(trial) + 1))*100)

done_and_crashed_no_norm <- bind_rows(done_and_crashed_norm, done_and_crashed_fake, done_and_crashed_invisible)
# no_norm = no normalisation

# factoring a crash/done
done_and_crashed_no_norm$success <- as.factor(done_and_crashed_no_norm$success)

# summarisiing to exclude unneeded rows
done_and_crashed_no_norm <- done_and_crashed_no_norm %>% 
  group_by(ID, trial, drift, success) %>%
  summarise_if(
    is.numeric,
    mean,
    na.rm = TRUE) %>%
  mutate(block  = case_when(drift == 'nodrift' ~ 'normal',
                            drift == 'fake' ~ 'fake',
                            drift == 'invisible' ~ 'invisible'))


# factoring
done_and_crashed_no_norm$block <- as.factor(done_and_crashed_no_norm$block)
done_and_crashed_no_norm <-   done_and_crashed_no_norm %>%     
  mutate(block = relevel(as.factor(block),  ref = "normal"))

#model
learn_effect <- glmer(success ~ trial_percentage * block + (1|ID), data= done_and_crashed_no_norm, family = binomial)
summary(learn_effect)

#plot
learn_effect_regr_pl <- interact_plot(learn_effect, pred = trial_percentage, modx = block, 
                                      #mod2 = N_visible_obstacles,
                                      vary.lty = FALSE,
                                      #interval = TRUE,
                                      modx.labels = c("Normal", "Fake", "Invisible"),
                                      x.label = "Block Progression (%)",
                                      y.label = "Success Probability",
                                      legend.main = "Block",
                                      main.title = "Success Probability through Block Progression")
