
XGB 
---
XGB is a distributed and greedy gradient boosting library in dkb+/q designed to be highly ***efficient*** . It is a native implementation in kdb+/q and does not depende on other module.
It implements machine learning algorithms under the [Gradient Boosting](https://en.wikipedia.org/wiki/Gradient_boosting) framework.
XGB provides a native distributed tree boosting that solve many data science problems in a fast and accurate way.
You can use xgb as long as the data is in memory or parted on disk.

Why do you reimplement an existing framework in kdb+/q and not just use xgboost (https://github.com/dmlc/xgboost) from Python/R inside kdb+/q?
--------------
Python/R require that the data fits into memory. The data size is the limitation. As long as the data is parted on disk you can apply xgb to your dataset. I am using it to participate the kaggle bosch (https://www.kaggle.com/c/bosch-production-line-performance) competition. 

Reference
---------
- Tianqi Chen and Carlos Guestrin. [XGBoost: A Scalable Tree Boosting System](http://arxiv.org/abs/1603.02754). In 22nd SIGKDD Conference on Knowledge Discovery and Data Mining, 2016 
