class CSV_classify(val train_filename : String, val test_filename : String, val label_cln : Int){
//load train and test data, prepare datasets:
val train_data = read.csv(train_filename, response = Some(new NominalAttribute("Class_label"), label_cln), missing = "NaN", header = true, rowNames = true):AttributeDataset
val test_data = read.csv(test_filename, response = Some(new NominalAttribute("Class_label"), label_cln), missing = "NaN", header = false, rowNames = true):AttributeDataset
val (xtr, ytr) = train_data.unzipInt  
val (xts, yts) = test_data.unzipInt 
//define accuracy of classification:
def accuracy_score(f: Array[Double] => Int) : Float = {
	var false_cases : Int = 0
	var k : Int = 0
	for(k <- 0 to (xts.length-1)){
	if(f(xts(k)) != yts(k)){
	false_cases = false_cases + 1
	}
	}
	return 1.toFloat-false_cases.toFloat/xts.length.toFloat
}
//Gaussian SVM classifier:
def gaussian_SVM(gamma: Double, coef : Double) : Float = {
    val svm = new SVM(new GaussianKernel(gamma), coef)
  	svm.learn(xtr, ytr)
  	svm.finish()
	return accuracy_score(svm.predict)
   }
//RandomForest classifier:
def rf(num_trees : Int) : Float = {
    val tree = new RandomForest(xtr, ytr, num_trees)
	return accuracy_score(tree.predict)
   }   
//list of all features:
def feature_names(): Array[smile.data.Attribute] = {
return train_data.attributes()
}
}
