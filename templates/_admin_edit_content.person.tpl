{% extends "admin_edit_widget_std.tpl" %}

{# display detected virtuals of user with id=id #}


{% block widget_title %}{_ Evercookie info _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
    {% for user_id  in  m.evercookie.clones[id] %}
	{% with m.rsc[user_id] as r %}
	    <a href="{% url admin_edit_rsc id=user_id %}">{% image r.depiction %} {{ r.title }}</a>,
	{% endwith %}
    {% empty %}
	This user has no detected virtuals.
    {% endfor %}
{% endblock %}
