
var websocket;
var kml;
var map;
var kml_urls = [];

function initialize() {

  /* initialize the google map */
  var mapOptions = {
        zoom: 7,
        center: new google.maps.LatLng(47.5, -120.5),
        mapTypeId: google.maps.MapTypeId.TERRAIN
      };

  map = new google.maps.Map(document.getElementById('map-canvas'), mapOptions);
  kml = new google.maps.KmlLayer({url: 'http://mathweb.ucdenver.edu/~mvejmelka/test.kmz', map: map});

  /* initialize the websocket subsystem */
  if(!("WebSocket" in window)) {
      sysmsg('<p><span style="color: red;">Websockets are NOT supported!</span></p>');
  } else {
      connect();
  };

  function connect()
  {
      websocket = new WebSocket('ws://127.0.0.1:8080/websocket/monitor');
      websocket.onopen = function(evt) { onOpen(evt) };
      websocket.onclose = function(evt) { onClose(evt) };
      websocket.onmessage = function(evt) { onMessage(evt) };
      websocket.onerror = function(evt) { onError(evt) };
  };

  function disconnect() {
      websocket.close();
  };


  function onOpen(evt) {
      sysmsg('CONNECTED\n');
  };

  function onClose(evt) { 
      sysmsg('DISCONNECTED\n');
  };

  function onMessage(evt) {
      sysmsg(evt.data + '\n');
  };

  function onError(evt) {
      sysmsg('ERROR ' + evt.data + '\n');
  };

  function sysmsg(code)
  {
    var ta = $('#sysmsg');
    var msg = moment().format('YYYY-MM-DD_HH:mm:ss') + " - " + code;
    ta.append(msg);
    ta.animate({scrollTop:ta[0].scrollHeight - ta.height() }, 1000);
  };

  $("#kml-slider").slider({
    value: 1, min:1, max:5, step:1, slide: function(ev, ui) { switchkml(ui.value); }
  });

  console.log("init done\n");
}


function switchkml(ndx)
{
  console.log("Switching kml file to " + ndx);
  if(kml != null) { kml.setMap(null); }
  kml = new google.maps.KmlLayer({url: 'http://mathweb.ucdenver.edu/~mvejmelka/test' + ndx + '.kmz', map: map});

}

google.maps.event.addDomListener(window, 'load', initialize);

