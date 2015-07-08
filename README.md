# A Trivial FTP Server

Documentation at <http://mor1.github.io/ocaml-tftp/>.

## Testing on OSX

Following
<http://www.weezey.com/2011/07/using-os-x-built-in-tftp-server/>:

    $ sudo launchctl load -F /System/Library/LaunchDaemons/tftp.plist

To change the served directory from the default of `/private/tftpboot`, edit the
`.plist` file and restart:

    $ sudo launchctl start com.apple.tftpd

## TODO

+ Sort out logging
+ Remove installed callbacks on final ACK
+ Add options support: blocksize, timeout, xfer size, window size
+ Ensure strings in (eg) error packets are valid NETASCII
+ Add some testing goodness
+ Requirements traceability using <https://github.com/infidel/reqtrace>
+ Write a client
+ Add Travis
