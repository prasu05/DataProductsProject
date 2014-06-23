#calculates distance of a point(repsented by rowindex)
#to all points
getDistance <- function(point, points){
    apply(points, 1, distance, point)    
}

#calculates euclidean distance b/w x & y
distance <- function(x, y){
    sum( (x-y)^2 )
}
