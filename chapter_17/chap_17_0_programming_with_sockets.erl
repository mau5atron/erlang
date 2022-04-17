% Chapter 17: Programming with Sockets

% Most of the more interesting programs that I write involve sockets one
% way or another. Programming with sockets is fun because it allows
% applications to interact with other machines on the internet, which has
% far more potential than just performing local operations.

% A socket is a communication channel that allows machines to communicate
% over the internet using the Internet Protocol (IP). In this chapter,
% we'll concentrate on the two core protocols of the Internet: Transmission
% Control Protocol (TCP) and User Datagram Protocol (UDP).

% UDP lets applications send short messages (called datagrams) to
% eachother, but there is no guarantee of delivery for these messages. They
% can also arrive out of order. TCP, on the other hand, provides a reliable
% stream of bytes that are delivered in order as long as the connection is
% established. Sending data by TCP incurs a larger overhead than sending
% data by UDP. You can choose between a reliable and slower channel (TCP)
% or a faster and unreliable channel (UDP).

% There are two main libraries for programming with sockets: get_tcp for
% programming TCP applications and gen_udp for programming UDP
% applications.

% In this chapter, we'll see how to program clients and servers using TCP
% and UDP sockets. We'll go through the different forms of servers that are
% possible (parallel, sequential, blocking, and nonblocking) and see how to
% program traffic-shaping applications that can control the flow of data to
% the application.