#/usr/bin/env python

import random
from locust import HttpLocust, TaskSet, task

class UserBehavior(TaskSet):
    @task(1)
    def index(self):
        self.client.get("/")
        self.client.get("/html/js/cone_reddit.mini.js")
        self.client.get("/html/fonts/glyphicons-halflings-regular.woff2")
        self.client.get("/iconlist")
        self.client.get("/stllist")
        self.client.get("/prefs")
        self.client.get("/icons?name=Turn.png")
        self.client.get("/model/full")

    @task(5)
    def move(self):
        self.client.get("/content?id=3")
        self.client.post("/model/event", {"data": "[0, 0]"})

        self.client.get("/content?id=4")
        self.client.post("/model/event", {"data": "[0, 0, 0]"})

        self.client.get("/content?id=5")
        self.client.post("/model/event", {"data": "[0, 0, 1]"})

    @task(5)
    def move_random(self):
        self.client.get("/content?id={}".format(random.randint(0, 10000)),
            name="/content?id=[id]")



class WebsiteUser(HttpLocust):
    task_set = UserBehavior
    min_wait=5000
    max_wait=9000
