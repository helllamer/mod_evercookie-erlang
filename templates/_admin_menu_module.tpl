{% if m.acl.is_admin %}
<li>
    {% if m.evercookie.can_admin %}
	<a href="{% url admin_mod_evercookie %}" {% ifequal selected "admin_mod_evercookie" %}class="current"{% endifequal %}>Evercookie</a>
    {% else %}
	<strike title="Evercookie is working but not adminable. Restart evercookie extension and look at error log.">Evercookie</strike>
    {% endif %}
</li>
{% endif %}
