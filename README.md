# alluvium

This service has two modes, a model training (train.R) and an API polling and prediction service (prediction_service.R).

To run prediction service
1) Turn on turbine data API.
2) In the project directory type command `./run.sh API_SERVICE`

To retrain model
1) In the project directory type command `./run.sh TRAIN`

NOTE: API polling rates and training params can be edited in config.yml
