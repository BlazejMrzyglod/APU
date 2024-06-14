library(neuralnet)
trainingInput <- as.data.frame(matrix(c(0,0.2,0.7,1,1.4,1.7,2,2.2,2.5,2.6, 2.8,3.2, 4, 4.1, 4.5,5,5.8,6)))
trainingOutput <- c(1,0.996, 0.841,0.596, 0.174, -0.131, -0.432, -0.537, -0.267,-0.045,0.486, 0.983, -0.996, -1.324, -4.569, 3.348,1.058, 1.011)
maxs <- apply(trainingInput[1], 2, max)
mins <- apply(trainingInput[1], 2, min)
trainingData <- cbind(trainingInput, trainingOutput)
colnames(trainingData) <- c("X", "Y")
result <- neuralnet(Y~X, trainingData, hidden=c(20,10), threshold = 0.01)
print(result$result.matrix["error",])
print(result)
plot(result)
testData <- as.data.frame(matrix(c(0.5, 1.1, 3.1, 4.2)))
net.results <-compute(result, testData)
ls(net.results)
print(net.results$net.result)

save(result,file="net.Rdata")

trainingInput <- as.data.frame(matrix(c(13.6,8,256,
                                       14.2, 18,512,
                                       15.3,8,512,
                                       13.3,8,256,
                                       13.3,16,256,
                                       13.6,8,256,
                                       13.3,8,256,
                                       13.6,8,256,
                                       13.6,16,256,
                                       13.6,8,256), nrow=10, ncol=3, byrow = TRUE))
trainingOutput <- c(4999,10199,8199,4579,5699,5999,4999,5999, 6349,5999)
maxs <- apply(trainingInput[1:3], 2, max)
mins <- apply(trainingInput[1:3], 2, min)
scaled.trainingInput <- as.data.frame(scale(trainingInput[1:3],center=mins,scale=maxs-mins))
trainingData <- cbind(scaled.trainingInput, trainingOutput)
colnames(trainingData) <- c("Ekran", "Ram", "Dysk","Cena")
net.cena <- neuralnet(Cena~Ekran+Ram+Dysk, trainingData, hidden=c(3,2), threshold = 0.01)
print(net.cena$result.matrix["error",])
print(net.cena)
plot(net.cena)
testData <- as.data.frame(matrix(c(13.6,8,256,
                                        14.2, 18,512,
                                        15.3,8,512,
                                        13.6,8,256), nrow=4, ncol=3, byrow = TRUE))
scaled.testData <- as.data.frame(scale(testData[1:3],center=mins,scale=maxs-mins))
net.results <-compute(net.cena, scaled.testData)
ls(net.results)
print(net.results$net.result)

