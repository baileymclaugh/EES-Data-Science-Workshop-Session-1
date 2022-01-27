##### Zach Wood, January 2022
##### Read and merge data from Wood et al 2019

# Read the data files
Fish <- read.csv("./Data/FishData.csv")
Lakes <- read.csv("./Data/LakeData.csv")

# Examine the data files
head(Fish)
head(Lakes)

# Check which columns the two data files have in common (should be 'Pop')
intersect(names(Fish),names(Lakes))

# Check length of data files prior to merge
nrow(Fish)
nrow(Lakes)

# Try a basic merge, specifying that we want to maintain 'Fish' as the dominant
#  dataframe
FishLakes <- merge(Fish,Lakes,all.x=T)

# Check that the merged dataframe has the appropriate columns
head(FishLakes)

# Check that the merged dataframe has the same number of rows as 'Fish'
nrow(FishLakes)

# Save the merged dataframe for later analysis
write.csv(FishLakes, "./Processed Data/FishLakes.csv")

# HI EVERYBODY