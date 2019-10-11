function uuidv4() {
  return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
    (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
  );
}

/* https://stackoverflow.com/a/21125098/945039 */
function getCookie(name) {
  var match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
  if (match) return match[2];
}

(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m);
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-149767044-1', 'auto');


/* If the browser_id hasn't already been set... */
if (document.cookie.indexOf('browser_uuid_set=1') == -1) {
  /* Generate a UUID, and assign it to the browser_id custom dimension */
  ga('set', 'dimension1', uuidv4());
  /* Set a cookie so we won't override the UUID we just set */
  document.cookie = 'browser_uuid_set=1; expires=Fri, 01 Jan 2100 12:00:00 UTC; domain=.arem.us; path=/';
}

ga('set', 'dimension2', new Date().getTime());
ga('send', 'pageview');

$(document).on('shiny:inputchanged', function(event) {
     if(['.clientdata_output_preview_hidden',
         '.clientdata_output_download_hidden'].indexOf(event.name) == -1){
           val = event.value; evn = event.name;
           valtype = Object.prototype.toString.call(val);
           /*if(valtype == '[object Object]'){ document.debugevent = event;
           }*/
           // if event value is non-atomic, replace with an atomic value
           if(event.name == 'infile'){
             val = Object.prototype.toSource.call(val[0]);
           } else if(event.name.indexOf('-input-click') != -1){
             evn = 'downloadfile'; val = 1;
           } else if(['[object Number]','[object String]','[object Boolean]']
            .indexOf(valtype) == -1){
              if(valtype == '[object Array]'){val = val.toString();
              } else{
                val = valtype;}
            }
            ga('set', 'dimension2', new Date().getTime());
            ga('send','event',evn, val, event.name, val);}
});