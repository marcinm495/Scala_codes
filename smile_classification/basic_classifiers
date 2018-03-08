def SVM_classifier_function(train_filename: String, test_filename: String, label_cln : Int) : Float = {
  	val train_data = read.csv(train_filename, response = Some(new NominalAttribute("Class_label"), label_cln), missing = "NaN", header = true, rowNames = true):AttributeDataset
  	val test_data = read.csv(test_filename, response = Some(new NominalAttribute("Class_label"), label_cln), missing = "NaN", header = false, rowNames = true):AttributeDataset
    val (xtr, ytr) = train_data.unzipInt  
    val (xts, yts) = test_data.unzipInt 
    val svm = new SVM(new GaussianKernel(8.0), 5.0)
  	svm.learn(xtr, ytr)
  	svm.finish()
	  var false_cases : Int = 0
	  var k : Int = 0
	  for(k <- 0 to (xts.length-1)){
	  if(svm.predict(xts(k)) != yts(k)){
	  false_cases = false_cases + 1
	  }
	  }
	  return 1.toFloat-false_cases.toFloat/xts.length.toFloat
   }
