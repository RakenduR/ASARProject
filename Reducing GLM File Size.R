stripGlmLR = function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}

getModelSize = function(n) {
  data = synthFrame(n)
  model = stripGlmLR(glm(y~xN+xC,data=data,
                         family=binomial(link='logit'),
                         y=FALSE, model=FALSE))
  length(serialize(model, NULL))
}

size3 = sapply(ndata, FUN=getModelSize)

ggplot(data.frame(n=ndata, modelsize=size3), aes(x=n, y=modelsize)) +
  geom_point() + geom_line()

# https://win-vector.com/2014/05/30/trimming-the-fat-from-glm-models-in-r/