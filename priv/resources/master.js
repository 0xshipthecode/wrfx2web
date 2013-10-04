
$(document)
  .ready(function() {

      $('.system.menu .item').tab();
      $('.ui.accordion').accordion();

      $('#wafirejob').click(function() {
        $('#content').html('<div class="ui segment fluid"><h3 class="ui header"> \
            Wildfire behavior simulation in Washington state \
          </h3> \
          <p>This job type requires the user to specify one or more ignition points \
             in addition to information required to submit a weather job. \
          </p> \
          <p> \
             The fire job is set up to simulate fire in central Washington state. \
          </p> \
          <p> \
             The time of ignition is fixed but the ignition location(s) can be modified \
             by the user. \
          </p> \
          <div class="ui floated right positive button"> \
            Begin \
          </div></div>'); });
      
      $('#coweatherjob').click(function() {
        $('#content').html('<div class="ui fluid segment"> \
          <h3 class="ui header">Forecast Colorado weather</h3> \
          <p>This job is set up to forecast the weather for 36 hours in Colorado \
             on a 2km domain. \
          </p> \
          <p>In addition to the standard weather forecast, a fuel moisture forecast \
             is also run.  Data assimilation for fuel moisture is conducted when \
             possible. \
          </p> \
          <p>The forecast length and start time can be modified by the user. \
          </p> \
          <div class="ui floated right positive button"> \
            Begin \
          </div></div>'); });


      $('#wafirejob').click();
  });
