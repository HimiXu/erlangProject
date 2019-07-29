# erlangProject
**Back-end Interface to Graphics


![alt text](https://i.imgur.com/Nf6f7w4.jpg)





The packet build: {Missiles,AntiMissiles,Cities,Radars,Launchers,Explosions,Interceptions}

Every item in every list description:

Missiles: {Ref, {falling, Velocity, {PositionX,PositionY}, Angle}}

Anti-missiles: {Ref, {intercepting, Velocity, {PositionX,PositionY}, Angle}}

Cities: {Name, {Status, {PositionX,PositionY}}

Launchers: {Ref, {Status, {PositionX,PositionY}}

Radars: {Ref, {Status, {PositionX,PositionY}}

Explosions: {PositionX,PositionY}

Interceptions: {PositionX,PositionY}

* Status = alive / destroyed
* Ref is a referance number



**Back-end diagrams
![alt text](https://i.imgur.com/ECGi9Bq.jpg)
