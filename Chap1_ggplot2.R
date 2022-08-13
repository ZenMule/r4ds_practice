library(tidyverse)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=cyl, y=hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=class, y=hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, alpha=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy), color = "blue", shape = 2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_wrap(~ class, nrow=2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(. ~ class)

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, linetype = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(mapping=aes(x=displ,y=hwy,color=drv),show.legend = F)

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(x=displ,y=hwy,color=drv))

ggplot(data = mpg, mapping = aes(x=displ,y=hwy)) +
  geom_smooth() +
  geom_point(mapping=aes(color=drv),show.legend = F)

ggplot(data=mpg, mapping = aes(x=displ,y=hwy))+
  geom_point(mapping = aes(color = class))+
  geom_smooth(data=filter(mpg,class=="subcompact"),se=F)


ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) +
  geom_point()+
  geom_smooth()

ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) +
  geom_point()+
  geom_smooth(mapping=aes(group=drv))

ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv)) +
  geom_point() +
  geom_smooth()

ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) +
  geom_point(mapping=aes(color=drv)) +
  geom_smooth()

ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) +
  geom_point(mapping=aes(color=drv)) +
  geom_smooth()

ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) +
  geom_point(mapping=aes(color=drv)) +
  geom_smooth(mapping=aes(linetype=drv))

ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) +
  geom_point(mapping=aes(color=drv))


ggplot(data = diamonds) +
  geom_bar(mapping=aes(x=cut))

demo <- tribble(
  ~a, ~b,
  "bar_1",20,
  "bar_2",30,
  "bar_3",40
)

ggplot(data = demo) +
  geom_bar(mapping=aes(x=a, y=b),stat="identity")

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,y=..prop..,group=1))

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,fill=clarity),position="identity",alpha=1/5)

ggplot(data=diamonds,mapping=aes(x=cut,fill=clarity))+
  geom_bar(position="fill",alpha=0.75)

ggplot(data=diamonds,mapping=aes(x=cut,fill=clarity))+
  geom_bar(position="dodge",alpha=0.75)

ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ,y=hwy),position="jitter")
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ,y=hwy))
ggplot(data=mpg) +
  geom_jitter(mapping=aes(x=displ,y=hwy))


ggplot(data=mpg,mapping=aes(x=cty,y=hwy))+
  geom_jitter()

ggplot(data=mpg,mapping=aes(x=cty,y=hwy))+
  geom_count()

