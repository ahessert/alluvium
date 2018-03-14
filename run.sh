R -f requirements.R --no-save;

RUN_MODE=$1
echo $RUN_MODE

if [ "$RUN_MODE" == 'API_SERVICE' ]; then
  R -f prediction_service.R --no-save
  
elif [ "$RUN_MODE" == 'TRAIN' ]; then
  R -f train.R --no-save
  
else
  echo "Please specify API_SERVICE or TRAIN as argument"
fi