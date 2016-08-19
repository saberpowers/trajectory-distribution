Jointly predicting exit velocity and launch angle for batter-pitcher matchups
=============================================================================

Two years into the Statcast era, much work has been done on understanding the
value of a batted ball, based on its trajectory. For example, on the first day
of this year's Saberseminar Glenn Healey discussed the intrinsic value of a
batted ball, modelling run value as a function of exit velocity, launch angle
and batted ball direction.

What has not received any attention publicly is the probability distribution of
batted ball trajectories. Understanding this distribution could inform fieder
position and batter/pitcher evaluation. In this project, presented at
Saberseminar 2016, I present an early attempt at methodology for predicting
the joint distribution of exit velocity and launch angle, given the batter and
pitcher involved in the matchup.

You can easily explore the results for the batter and pitcher of your choosing
at https://saberpowers.shinyapps.io/trajectory-distribution.

This project is far from complete. Some potential improvements are:

  * As Alan Nathan pointed out to me at the FanGraphs meetup on the Friday
  before Saberseminar, (roughly) 10 degrees will not be the optimal launch
  angle for all batters. Batters whose swing plane is 10 degrees probably have
  an optimal launch angle close to 10 degrees. Batters whose swing plane is 25
  degrees probably have an optimal launch angle closer to 25 degrees.

  * I ignore pitch type and location, but obviously these are two variables
  that strongly inform launch angle.

  * I have presented this model, but I have not presented any validation of it.
  That is coming.

  * The next thing to incorporate is batted ball direction (horizontal launch
  angle). This is a necessary step if this is ever going to be used for fielder
  positioning.

  * Also worth including are data on non-batted balls. For example, I currently
  ignore swing-and-misses because there is no way to define exit velocity or
  launch angle on these swings. But combining these data with batted ball data
  are necessary to get a complete picture of the player.

To conclude, this is early stage research with much room for improvement. There
are aspects of the currently methodology which I find unsatisfactory. If you
are interested in this endeavour or have suggestions for improving some of the
methodology, please reach out to me.

