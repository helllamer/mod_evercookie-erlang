{% if m.acl.is_admin %}
<li><a href="{% url admin_mod_evercookie %}" {% ifequal selected "admin_mod_evercookie" %}class="current"{% endifequal %}>Evercookie</a></li>
{% endif %}
