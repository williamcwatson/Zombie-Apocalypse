# Escape Zombie Land!
 
# This is a simulation an escape from a hot zombie zone.  It freezes and gives an error if you get get killed so you had best not.  You attempt to navigate the zone by constructing waypoints.
 
# This is not a very clean set up and I would like to clean it up.  However, I have spent way more time on it than I intended.  So I might come back to it another day.


# Zombies are  distributed on a 10 x 10 grid.
gridxy = c(10,10)
 
# The number of zombies on the map
nzombies = 40
 
# How close a zombie needs to be to take out a human is defined here 
same.space = .05
 
# This is how close a human needs to be to consider that the human has reached the waypoint.
waypoint.hit = .2
 
# I set up the zombie distribution randomly initially.
set.seed(1)
zombiexy = cbind(runif(nzombies)*gridxy[1], runif(nzombies)*gridxy[2])
plot(zombiexy, main="Zombies!", xlab="X", ylab="Y", col=grey(.2), xlim=c(0,gridxy[1]), ylim=c(0,gridxy[2]))
 
# Humans
startpoint = c(.5,.5)
humans = data.frame(x=c(0,-.25, .25), y=c(0,.25, -.25), name=c("You","Pete", "Jimmy"))
 
humansxy = humans[,1:2]
 
# Count humans
nhumans = nrow(humansxy)
 
(humansxy = humansxy+rep(startpoint, each=nhumans))
 
# Plot humans
points(humansxy, pch=8)
 
# Safety
safety = c(9.5,9.5)
 
# Waypoints, specify the waypoints the humans are to take to get to the destination.
waypoints = rbind(c(2.5,2), c(5,6), c(9.75, 7))
 
# Route
route = rbind(startpoint, waypoints, safety, safety, safety)
lines(route)
 
# A vector that will be shortenned as the simulation progresses
route.unreached = route
 
points(safety[1], safety[2], pch=7)
 
# Now let's imagine that each zombie has a sensory distance in which the zombie can detect humans.
detection.dist = 3
 
# How fast the zombies can move.  Zombies have no inertia.
zombie.acceleration = .075
 
# How fast humans can move
human.acceleration = .075
 
# Humans can outrun zombies by building up inertia
human.inertia = .5
 
# Initially everybody is at rest.
hmovement = zmovement = 0
 
# ---------------------------------------------------                       
#### Set up a single loop to check programming.
 
# First the zombies move
 
# First let's check how close each zombie is to each human.
# We will accomplish this by going through each zombie and checking how far away each zombie is from each human.
  distances = matrix(NA, nrow=nzombies, ncol=nhumans)
for (i in 1:nzombies) for (ii in 1:nhumans) distances[i,ii] = (sum((zombiexy[i,]-humansxy[ii,])^2))^.5
 
  target =  matrix(1:nrow(humansxy), ncol=nzombies, nrow=nrow(humansxy))[apply(distances, 1, order)[1,]]
# The apply command will apply the order command to each row while the [1,] selects only the critter that is closes.
 
plot(zombiexy, xlab = "X", ylab = "Y", main="If zombies did not have perception limitations")
for (i in 1:nzombies) arrows(x0=zombiexy[i,1], y0=zombiexy[i,2],
                                x1=humansxy[target,][i,1], 
                                y1=humansxy[target,][i,2], 
length=.1, col="red")

  points(humansxy, pch=8)
 
# Safety
points(9.5,9.5, pch=7)
 
# However, if the target is outside of detection range then zombies cannot target that human.
  target[distances[cbind(1:nzombies,target)]>detection.dist]=NA
 
# Plot the relationship between zombies and humans
plot(zombiexy, xlab = "X", ylab = "Y", main="Escape Zombie Land")
for (i in 1:nzombies) arrows(x0=zombiexy[i,1], y0=zombiexy[i,2],
                                x1=humansxy[target,][i,1], 
                                y1=humansxy[target,][i,2], 
length=.1, col="red")
# Plot humans
points(humansxy, pch=8)
 
# Safety
points(9.5,9.5, pch=7)
 
 
# This calculates the difference between the current position of each zombie and that of the closest human.
  ab = zombiexy-humansxy[target,]
 
  ab=ab[!is.na(target),]
 
# Now calculate the difference in the horizontal and vertical axes that the zombies will move as a projection into the direction of the closest zombie outside of the perceptive zone.
  a.prime = zombie.acceleration/(1 + (ab[,2]^2)/(ab[,1]^2))^.5
  b.prime = (zombie.acceleration^2-a.prime^2)^.5
 
# This corrects the movement to ensure that the zombies are moving at the humans rather than away from them.
  zmovement = cbind(a.prime * sign(ab[,2]), b.prime * sign(ab[,1]))
  between = function(xy1,xy2,point) ((point>xy1&point<xy2)|(point>xy2&point<xy1))
  zmovement = zmovement*(-1)^between(zombiexy[!is.na(target),],humansxy[target[!is.na(target)],], zombiexy[!is.na(target),]-zmovement)
 
# Set the new xypos
(zombiexy[!is.na(target),] = zombiexy[!is.na(target),]+zmovement)
 
points(zombiexy, col="red")
 
# Check if any of the zombies caught a human
  distances = matrix(NA, nrow=nzombies, ncol=nhumans)
for (i in 1:nzombies) for (ii in 1:nhumans) distances[i,ii] = (sum((zombiexy[i,]-humansxy[ii,])^2))^.5
 
  zombie.feast = distances[cbind(1:nzombies,target)]<same.space
  zombie.feast[is.na(zombie.feast)]=F
 
  humans.down=NULL
(humans.down=unique(c(humans.down, unique(target[zombie.feast]))))
 
# Remove victorious zombies from zombie pool (occupied)
(zombiexynew = zombiexy[!zombie.feast,])
 
# Check if you are eaten
if (1 %in% humans.down) stop("You died")
 
# Display messages:
if (length(humans.down)==1) warntxt = paste(humans[humans.down,3], "'s down!", sep="")
if (length(humans.down)>1) warntxt = paste(humans[humans.down,3], "are down!")
 
# Remove any "captured" humans
if (length(humans.down)>0) { 
    humansxy = humansxy[-humans.down,]
    nhumans = nrow(humansxy)
}
 
# Now the surving humans get to move.
 
# However, we only calculate the movement for the leader (you) since all of the other humans move in parrellel to you.
 
# Movement is also much simpler since humans just run from one waypoint to the next.
 
# First we check if we have reached any waypoints (which we have since we start on one).
  way.distance = 
(sum((humansxy[1,]-route.unreached[1,])^2))^.5
 
if (length(route.unreached)==0) stop("Congraduations! Safety reached!")
 
if (way.distance<waypoint.hit) (route.unreached = route.unreached[-1,])
 
# Now calculate the next place to move
  ab = humansxy[1,]-route.unreached[1,]
 
# Now calculate the difference in the horizontal and vertical axes that the humans will move as a projection into the direction of the closest human outside of the perceptive zone.
  a.prime = human.acceleration/(1 + (ab[,2]^2)/(ab[,1]^2))^.5
  b.prime = (human.acceleration^2-a.prime^2)^.5
 
# This corrects the movement to ensure that the zombies are moving at the humans rather than away from them.
  hmovement = cbind(a.prime * sign(ab[,2]), b.prime * sign(ab[,1]))
  between = function(xy1,xy2,point) ((point>xy1&point<xy2)|(point>xy2&point<xy1))
  hmovement = hmovement*(-1)^between(humansxy[1,],route.unreached[1,], humansxy[1,]-hmovement)
 
# Let's see what this looks like!
points(humansxy, pch=8)
lines(route)
points(safety[1], safety[2], pch=7)
 
points(route.unreached[-nrow(route.unreached),], pch=17)
 
# Set the new xypos
(humansxy = humansxy+ t(matrix(hmovement,nrow=2, ncol=nhumans)))
# hmovement0 will save the movement to allow for inertia
  hmovement0 = t(matrix(hmovement,nrow=2, ncol=nhumans))
points(humansxy, pch=8, col="blue")
 
# ------------------------------------------------------
# Let's turn this into an animation.
ani.pause=F
 
library(animation)
flocking = function (ani.pause=F) {
 
# This is text displayed on the map initially
  warntxt = "We need to make a run for the safe zone.  Choose a route."
 
while (nrow(route.unreached)>2) {
 
# First let's check how close each zombie is to each human.
# We will accomplish this by going through each zombie and checking how far away each zombie is from each human.
  distances = matrix(NA, nrow=nzombies, ncol=nhumans)
for (i in 1:nzombies) for (ii in 1:nhumans) distances[i,ii] = (sum((zombiexy[i,]-humansxy[ii,])^2))^.5
 
if (nrow(humansxy)>1) target = matrix(1:nrow(humansxy), ncol=nzombies, nrow=nrow(humansxy))[apply(distances, 1, order)[1,]]
if (nrow(humansxy)==1) matrix(1, ncol=nzombies, nrow=1)
# The apply command will apply the order command to each row while the [1,] selects only the critter that is closes.
 
  target[distances[cbind(1:nzombies,target)]>detection.dist]=NA
 
# Plot the relationship between zombies and humans
plot(0,0, type="n", xlab = "X", ylab = "Y", main="Escape Zombie Land", xlim=c(0,gridxy[1]), ylim=c(0,gridxy[2]))
 
# Safety
points(9.5,9.5, pch=7)
 
text(5,.25,warntxt)
 
 
# This calculates the difference between the current position of each zombie and that of the closest human.
  ab = zombiexy-humansxy[target,]
 
  ab=ab[!is.na(target),]
 
# Now calculate the difference in the horizontal and vertical axes that the zombies will move as a projection into the direction of the closest zombie outside of the perceptive zone.
  a.prime = zombie.acceleration/(1 + (ab[,2]^2)/(ab[,1]^2))^.5
  b.prime = (zombie.acceleration^2-a.prime^2)^.5
 
# This corrects the movement to ensure that the zombies are moving at the humans rather than away from them.
  zmovement = cbind(a.prime * sign(ab[,2]), b.prime * sign(ab[,1]))
  between = function(xy1,xy2,point) ((point>xy1&point<xy2)|(point>xy2&point<xy1))
  zmovement = zmovement*(-1)^between(zombiexy[!is.na(target),],humansxy[target[!is.na(target)],], zombiexy[!is.na(target),]-zmovement)
 
# Set the new xypos
  zombiexy[!is.na(target),] = zombiexy[!is.na(target),]+zmovement
 
points(zombiexy)
 
 
# Check if any of the zombies caught a human
  distances = matrix(NA, nrow=nzombies, ncol=nhumans)
for (i in 1:nzombies) for (ii in 1:nhumans) distances[i,ii] = (sum((zombiexy[i,]-humansxy[ii,])^2))^.5
 
  zombie.feast = distances[cbind(1:nzombies,target)]<same.space
  zombie.feast[is.na(zombie.feast)]=F
 
  humans.down=NULL
  humans.down=unique(c(humans.down, unique(target[zombie.feast])))
 
# Remove victorious zombies from zombie pool (occupied)
  zombiexynew = zombiexy[!zombie.feast,]
 
# Check if you are eaten
if (1 %in% humans.down) warntxt = "You died"
 
# Display messages:
if (length(humans.down)==1) warntxt = paste(humans[humans.down,3], "'s down!", sep="")
if (length(humans.down)>1) warntxt = paste(humans[humans.down,3], "are down!")
 
# Remove any "captured" humans
if (length(humans.down)>0) { 
    humansxy = humansxy[-humans.down,]
    nhumans = nrow(humansxy)
}
 
# Now the surving humans get to move.
 
# However, we only calculate the movement for the leader (you) since all of the other humans move in parrellel to you.
 
# Movement is also much simpler since humans just run from one waypoint to the next.
 
# First we check if we have reached any waypoints (which we have since we start on one).
  way.distance = (sum((humansxy[1,]-route.unreached[1,])^2))^.5
 
if (length(route.unreached)==0) stop("Congraduations! Safety reached!")
 
if (way.distance<waypoint.hit) (route.unreached = route.unreached[-1,])
 
# Now calculate the next place to move
  ab = humansxy[1,]-route.unreached[1,]
 
# Now calculate the difference in the horizontal and vertical axes that the humans will move as a projection into the direction of the closest human outside of the perceptive zone.
  a.prime = human.acceleration/(1 + (ab[,2]^2)/(ab[,1]^2))^.5
  b.prime = (human.acceleration^2-a.prime^2)^.5
 
# This corrects the movement to ensure that the zombies are moving at the humans rather than away from them.
  hmovement = cbind(a.prime * sign(ab[,2]), b.prime * sign(ab[,1]))
  between = function(xy1,xy2,point) ((point>xy1&point<xy2)|(point>xy2&point<xy1))
  hmovement = hmovement*(-1)^between(humansxy[1,],route.unreached[1,], humansxy[1,]-hmovement)
 
# Let's see what this looks like!
points(safety[1], safety[2], pch=7)
 
points(route.unreached[-nrow(route.unreached),], pch=17)
 
# Set the new xypos
  humansxy = humansxy+ t(matrix(hmovement,nrow=2, ncol=nhumans))+hmovement0*human.inertia
# hmovement0 will save the movement to allow for inertia
  hmovement0 = t(matrix(hmovement,nrow=2, ncol=nhumans))
points(humansxy, pch=8, col="blue")
 
# This is only used in the event that the animate package is in use.
if (ani.pause) ani.pause()
}
}
ani.options(interval = .15)
flocking()
# Let's see how we do at escaping zombie land
ani.options(ani.width=600, ani.height=600, interval=.25)
saveGIF(flocking( ani.pause=T), movie.name = "Zombies.gif", replace=T)
