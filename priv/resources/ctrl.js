
var url_base = location.href.substring(7, location.href.indexOf('pages') - 1);
var websocket;

function initialize() {

  /* initialize the google map */
  var mapOptions = {
        zoom: 7,
        center: new google.maps.LatLng(47.5, -120.5),
        mapTypeId: google.maps.MapTypeId.TERRAIN
      };

  var map = new google.maps.Map(document.getElementById('map-canvas'), mapOptions);
  var marker = null;
  google.maps.event.addListener(map, 'click',
                                function(event) {
                                  if(marker != null) { marker.setMap(null); }
                                  marker = new google.maps.Marker({ position: event.latLng, map : map, title : 'Ignition point', draggable : true });
                                  google.maps.event.addListener(marker, 'dragend', function(event) { updateIgnCoords(event.latLng); } );
                                  updateIgnCoords(event.latLng);
                                });

  /* initialize the websocket subsystem */
  if(!("WebSocket" in window)) {
      sysmsg('<p><span style="color: red;">Websockets are NOT supported!</span></p>');
  } else {
      connect();
  };

  function updateIgnCoords(ll)
  {
    $('#ign_lat').val(ll.lat().toPrecision(7));
    $('#ign_lon').val(ll.lng().toPrecision(7));
  };

  function connect()
  {
      websocket = new WebSocket('ws://' + url_base + '/websocket/control');
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
    var json = JSON.parse(evt.data);
    if(json['action'] == 'submit') {
      if(json['result'] == 'success') {
        sysmsg('SUCCESS! Stand by for redirect to monitoring page.');
        setTimeout(function () { window.location.href = "http://mathweb.ucdenver.edu/~mvejmelka/fbs/pages/monitor.html?jobid=" + json['jobid']; }, 5000);
      } else {
        sysmsg('Submit action failed with reason: ' + json['error']);
        $("#submit").attr("disabled", false);
      }
    } else if(json['action'] == 'state_update') {
      $("#host").text(json['system']);
      $("#freenodes").text(json['freenodes']);
      $("#nodes").text(json['nodes']);
      $("#qlen").text(json['qlen']);
      $("#numsims").text(json['numsims']);
      $("#activesims").text(json['activesims']);
      $("#lastupdated").text(json['lastupdated']);
    } else if(json['action'] == 'display') {
        sysmsg(json['message']);
    } else {
      sysmsg('JSON message <' + evt.data + '> not understood.');
    }
  };

  function onError(evt) {
      sysmsg('ERROR ' + evt.data + '\n');
  };

}

function sysmsg(code)
{
  var ta = $('#sysmsg');
  var msg = moment().format('YYYY-MM-DD_HH:mm:ss') + " - " + code + '\n';
  ta.append(msg);
  ta.animate({scrollTop:ta[0].scrollHeight - ta.height() }, 1000);
}


google.maps.event.addDomListener(window, 'load', initialize);

  function submitjob() {
      if(websocket.readyState == websocket.OPEN) {
          sysmsg('Submitting job request now.');
          websocket.send('{ "request": "submit", ' +
                         '"lat": ' + $('#ign_lat').val() + ", " +
                         '"lon": ' + $('#ign_lon').val() + ", " +
                         '"time": "' +$('#ign_time').val() + '", ' +
                         '"fc_len": ' + $('#fc_len').val() +
                         ' }');
          $('#submit').attr("disabled", true);
      } else {
           sysmsg('Websocket disconnected.');
      };
  };


