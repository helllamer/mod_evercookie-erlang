{% extends "admin_edit_widget_std.tpl" %}

{# display list of detected virtuals #}


{% block widget_title %}{_ All users with clones _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
<ul class="short-list">
    {% for user_ids  in  m.evercookie.clones.all %}
	<li class="clearfix">
	    {% for user_id  in  user_ids %}
		{% with m.rsc[user_id] as r %}
		    <a href="{% url admin_edit_rsc id=user_id %}">{% image r.depiction %} {{ r.title }}</a>
		    -
		{% endwith %}
	    {% endfor %}
	</li>
    {% empty %}
	<li><i>There are no virtuals for all users.</i></li>
    {% endfor %}
</ul>
{% endblock %}
