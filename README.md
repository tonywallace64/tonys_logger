# tonys_logger
parallel otp logger

High performance OTP logger.  This logging system consists of writers under supervision, a name server under supervision, and client code.
When a writer is started it is registered with the name server that stores that writer's Pid in a shared ets table. Clients
can then send log requests directly to the correct writer.

This design removes the server from between the source of the requests and the writers that save them to disk making the process
efficient and removing potential bottlenecks.
