
var websocket;
var kml = null;
var map = null;
var kml_urls = [];
var job_id = null;
var last_stage = null;

function initialize() {

  /* parse the location.href and find the jobid if any */
  ndx = location.href.indexOf('jobid');
  if(ndx >= 0) {
    job_id = location.href.substring(ndx + 6);
  }

  /* initialize the google map */
  var mapOptions = {
        zoom: 7,
        center: new google.maps.LatLng(47.5, -120.5),
        mapTypeId: google.maps.MapTypeId.TERRAIN
      };

  map = new google.maps.Map(document.getElementById('map-canvas'), mapOptions);

  /* initialize the websocket subsystem */
  if(!("WebSocket" in window)) {
      sysmsg('<p><span style="color: red;">Websockets are NOT supported!</span></p>');
  } else {
      connect();
  };


  function connect()
  {
      websocket = new WebSocket('ws://127.0.0.1:8000/websocket/monitor');
      websocket.onopen = function(evt) { onOpen(evt) };
      websocket.onclose = function(evt) { onClose(evt) };
      websocket.onmessage = function(evt) { onMessage(evt) };
      websocket.onerror = function(evt) { onError(evt) };
  };


  function disconnect() {
      websocket.close();
  };


  function onOpen(evt) {
      sysmsg('CONNECTED');
      sysmsg('You are monitoring job ' + job_id);
      
      // send a request to monitor job with our job_id
      websocket.send("{ \"request\" : \"monitor\", \"jobid\" : \"" + job_id + "\" }");
  };

  function onClose(evt) {
      sysmsg('DISCONNECTED');
  };

  function onMessage(evt) {
      console.log(evt.data);
      json = JSON.parse(evt.data);
      console.log(json);
      if(json["action"] == "update_status") {
        $("#comp-stage").val(json["stage"]);
        $("#comp-time").val(json["completion_time"]);
        $("#sim-time").val(json["sim_time"]);
        $("#sim-accel").val(json["sim_accel"]);
        if(json["stage"] != last_stage) {
            sysmsg('Simulation in stage \"' + json["stage"] + "\".");
            last_stage = json["stage"];
         }
        if(json.hasOwnProperty("kmls")) {
            kml_urls = json["kmls"];
            $("#kml-slider").slider('option',{max: kml_urls.length});
            $("#comp-progress").progressbar({value : json["percent_done"]});
            if(kml_urls.length == 1) { switchkml(1); }
        }
      } 
  };

  function onError(evt) {
      sysmsg('ERROR ' + evt.data);
  };

    $("#kml-slider").slider({
    value: 1, min:1, max:5, step:1, slide: function(ev, ui) { switchkml(ui.value); }
  });

    $("#comp-progress").progressbar("option", "value", false);
}



function sysmsg(code)
  {
    var ta = $('#sysmsg');
    var msg = moment().format('YYYY-MM-DD_HH:mm:ss') + " - " + code + '\n';
    ta.append(msg);
    ta.animate({scrollTop:ta[0].scrollHeight - ta.height() }, 1000);
  };



function switchkml(ndx)
{
  if(kml != null) { kml.setMap(null); }
  var uri = 'http://mathweb.ucdenver.edu/~mvejmelka/fbs/kmls/' + job_id + "/" + kml_urls[ndx-1];
  kml = new google.maps.KmlLayer({url: uri, map: map});
  $("#curr_time").val(kml_urls[ndx-1].substr(0, 19));
}

google.maps.event.addDomListener(window, 'load', initialize);

