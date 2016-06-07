#/usr/bin/env python
from locust import HttpLocust, TaskSet, task

class UserBehavior(TaskSet):
    @task(1)
    def index(self):
        self.client.get("/")
        self.client.get("/html/js/Main.js")
        self.client.get("/iconlist")
        self.client.get("/icons?name=Turn.png")
        self.client.get("/model/full")

class WebsiteUser(HttpLocust):
    task_set = UserBehavior
    min_wait=5000
    max_wait=9000
