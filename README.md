# tonys_logger
<h2>parallel otp logger</h2>

High performance OTP logger.  This logging system consists of writers under supervision, a name server under supervision, and client code.
When a writer is started it is registered with the name server that stores that writer's Pid in a shared ets table. Clients
can then send log requests directly to the correct writer.

This design removes the server from between the source of the requests and the writers that save them to disk making the process
efficient and removing potential bottlenecks.

The system supports synchronous and asynchronous logging operations.  A synchronous logging operation halts the client while the write occurs.  With a synchronous logging operation buffers are flushed before the call returns.  These calls are provided for situations where the logged information occurs infrequently and is very valuable, such as logging runtime errors.

Asynchronous logging is provided high volume low latency situations where occasional data loss is acceptable.  In these logging events the log operation is fire and forget.  The writer then uses a delayed write to maximise io throughput.  A use case where this strategy may be best is in logging webserver requests and replies.


<h2>Configuring the logger</h2>

The definition of the default logging setup is shown below:
<code><pre>
  [
     {writers,
        [
          {default,"log/default.log"}
        ]},
     {levels, 
       [
         {debug,default,nosync},
	       {info, default,nosync},
	       {error,default,sync}
       ]}
  ]
  </pre></code>
  In this configuration there is one writer that writes the file log/default.log.  Logging levels are debug, info and error.  Errors are logged synchronously.  These are application environment variables and can be set either in the .app file, or preferably in a tonys_logger.config file.  Let us redefine the setup to ignore debug messages.  Currently it is an error to not have a writer associated with a debug level.  The easy way to throw away data is to log it to /dev/null.  This is done by adding the new writer and pointing debug to it:
  <code><pre>
  [
     {writers,
        [
          {null,"/dev/null"},
          {default,"log/default.log"}
        ]},
     {levels, 
       [
         {debug,null,nosync},
	       {info, default,nosync},
	       {error,default,sync}
       ]}
  ]
  </pre></code>
  <h2>Using the logger</h2>
  
  Logging is done with the call:
  <code>
  tonys_logger:log(Level,Message)
 </code> 
  It would be usual practice, and consistent with other logging frameworks, to wrap this call in a macro set of macros so that:
  <code>
  tonys_logger:log(info,"My Message")
  </code>
  becomes:
  <code>
  ?info("My Message")
  </code>
  

  
