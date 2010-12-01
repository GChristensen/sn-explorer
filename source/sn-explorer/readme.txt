sn-explorer - a simple library for social network crawling.

(C) 2008-2010, g/christensen (gchristnsn@gmail.com)


To add support for the particular social network it necessary to implement web
client interface for this network. See section 3 for details.


1. High Level Interface

Package sn-explorer-user

[Function]
sn-explore network user password &key target-dir root-user load-graphics verbose (depth 1) (delay 0) => pathname

Returns pathname of the resulting Graphviz dot file which represents given 
subset (defined by traversal depth) of a social network structure from the
specified user point of view.

  network       social netwokr to log in, any simbol from 
                sn-explorer-client:*supported-social-networks* list

  user          agent (user in behalf of crawler works) login in the social 
                network

  password      agent password

  target-dir    pathname of the directory where to save resulting dot file and
                graphics

  root-user     root user identifier consumable by the current social network 
                cilent

  load-graphics generalized boolean which indicates should resulting visual 
                representation of user relation graph contain user pictures 

  verbose       write progress messages to stdout when set to t, generalized 
                boolean

  depth         network traversal depth (1 = only user direct friends and 
                so on)
  
  delay         delay between user profile transitions in seconds


[Special variable]
*sn-dot-program*

Specifies dot processing program from Graphviz installation used by 
`sn-dot->image' function (should be available through PATH env. variable).
Default is "neato".

[Function]
sn-dot->image dot-file-path image-type &optional parameters => pathname

Produces visual representation of a user relation graph as image of the 
specified type in the same directory as the input dot file using Graphviz.
Returns pathname of the resulting image file without device and directory
components.

  dot-file-path input dot file pathname

  image-type    resulting image type, string

  parameters    additional parameters to the dot processing program as a list
                of strings
  

2. Low Level Interface 


Package sn-explorer-crawler  

[Struct]
sn-user-node

Represents user from the crawler abstract point of view.

Fields:

  user      client user object (descentant of sn-user)

  level     distance in the social network from the root user (rank)

  adjacent  list of user ids adjacent to this user

  visited-p used by traversal algorithms

[Class]
sn-user-pool

Adjacency list representing social network graph structure in memory.

Fields:

  sn-adjacency-list hash-map which maps user id to the corresponding 
                    sn-user-node object

  sn-root-user-id   user id with rank 0 (from which crawling begins)


[Function]
sn-crawl-users session root-user-id depth &key group delay acceptorf => sn-user-pool

Web crawler main function. Returns social network graph in-memory 
representation.

  session      instance of client-specific session object (descendant of 
               sn-session)

  root-user-id id of the root user
 
  depth        network traversal depth (max. rank)

  delay        see `sn-explore'

  acceptorf    function of one argument which is being called after a 
               sn-user-node has been created

[Function]
traverse-user-pool user-pool acceptorf &key algorithm root-user-id depth delay => t

Visits each node in the given in-memory graph representation.

  user-pool    instance of sn-user-pool class being traversed

  acceptorf    function of one argument called on each graph node
  
  algorithm    :breadth-first and :depth-first algorithms are available
  
  root-user-id id of the root user (any id from the adjacency list) 
  
  depth        see `sn-crawl-users'
  
  delay        see `sn-crawl-users'


Package sn-explorer-graph

[Function]
sn-print-graph user-pool session directory delay acceptorf (load-graphics t) => pathname

Converts user relation graph in-memory representation into Graphviz dot file.
Returns resulting dot file pathname.

  user-pool     instance of sn-user-pool being processed

  session       instance of client-specific session object (descendant of 
                sn-session)

  directory     output dot file pathname

  delay         see `traverse-user-pool'

  acceptorf     function of one argument which is being called after each 
                user photo has been loaded

  load-graphics see `sn-explore'


3. Social network client interface


Package sn-explorer-client

[Class]
sn-session 

Represents open social network session which contains social network web site
cookies. Content of this class should be considered not touchable by client,
but client should provide its own descendant of this class even if it 
does not contain any new fields. Function `sn-login' then should return 
this client-defined descendant instance (see `sn-login').

[Class]
sn-user

Clien-specific user data container.

Fields:

  sn-user-id     object which uniquely identifies user in the social network

  sn-user-name   user name (string)

  sn-userpic-uri URL of user image (string)

  sn-user-data   arbitrary data associated with a class instance (alist)
                 use `sn-user-set-data', `sn-user-get-data' and `sn-user-rem-data'
                 functions to access this alist.

[Function]
sn-http-request session method url &key parameters (redirect t) => body headers status-code

Tiny wrapper around drakma:http-session which internally uses cookies from 
given sn-session object. Social network client should use this function 
to access social network web pages.

  session    client-specific sn-session instance returned by `sn-login' 
             (see `sn-login')

  method     see `drakma:http-request'

  url        see `drakma:http-request'

  parameters see `drakma:http-request'

  redirect   see `drakma:http-request'

Return values:

  body        web page content

  headers     response http headers

  status-code HTTP status code 

[Special variable]
*supported-social-networks*

List of symbols identifying social networks supported by sn-explorer library.
Any social network client implementation should add symbol uniquely identifying
its social network to this list.

[Generic Function]
sn-login network &key => sn-session

Implementation of this method should obtain social network cookies and return 
new instance of sn-session class. In general, to obtain cookies it only 
necessary to make request to the social network login page by sn-http-request
function with fresh instance of sn-session client-defined descendant. 
It also necessary to check that login was successful and return nil otherwise.
Returned object then will be passed to the other client methods as a `session' 
parameter.

network a symbol from *supported-social-networks* list; necessary only for
        generic function dispatch and may be ignored by the method 
        implementation

[Generic Function]
sn-extract-user-id session page-url => object

Extracts user id from the user profile or other user related page.
User id could be a string or a number or any object which could be transformed
to string uniquely identifying user in the social network.

  session  social network session
  page-url URL of the user profile or other user related page from which id
           could be obtained

[Generic Function]
sn-agent-id session => object

Retrieves agent (user in behalf of which crawler works) user-id.

[Generic Function]
sn-logout session => nil

Performs logout operation.

[Generic Function]
sn-user-link session user-id => string

Returns a profile page URL of the user identified by the given user-id.

[Generic Function]
sn-id->string session user-id => string

Transforms client specific representation of the user-id object to string.
Resulting string should only contain ASCII characters.

[Generic Function]
sn-instantiate-user session user-id => sn-user

This method should obtain user data from the social network by the given user-id 
and return fresh isntance of a sn-user object with at least two first fields 
filled.

[Generic Function]
sn-adjacent-users session user-id => list

This method should return list filled with IDs of direct friends of the user 
defined by the given user-id.


You can use sn-explorer-client-vkontakte implementation and sn-explorer-gui 
library as usage example of the API described above.
