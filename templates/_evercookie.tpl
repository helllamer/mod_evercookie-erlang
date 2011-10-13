<script type="text/javascript">
  
  var _cookievalue = '{{ m.evercookie.new }}';
  var _cookiename = '{{ m.evercookie.name.png }}';
  
  var _evercookie_png_resource = '{% url evercookie_png %}';
  var _evercookie_cache_resource = '{% url evercookie_cache %}';
  var _evercookie_etag_resource = '{% url evercookie_etag %}';
  
  var ec = new evercookie();
  
  _cookievalue = 'trusted_cookie_value';
  
  //ec.set(_cookiename, _cookievalue);
  
  setTimeout(function()
  {
  	ec.get(_cookiename, function(best, all) {});
  },5000);
  
</script>
