mod\_s3\_upload: XEP-0363 with S3-compatible storage
====================================================

* Author: Roman Hargrave <roman@hargrave.info>

Implements HTTP Upload using any S3-compatible storage service.

# OTP Compatibility

This module requires Erlang/OTP 25.0 or higher, as it depends heavily
on the `uri_string` module to implement URL signing.

# How it works

The S3 API is highly compatible with XEP-0363 because it uses PUT and
GET for object placement and retrieval. What's more, a client may be
provided with a URL that may be used to upload a specific file without
having to expose API credentials. This makes for an extremely
desirable XEP-0363 storage backend.

An outline of an XEP-0363 transaction using this module follows:

1. A client sends a slot-request IQ to the upload service
2. The server verifies that the client may upload files, and that the
   proposed file size is acceptable
3. The server generates an object URL, which will be used by clients
   to download the file once it has been uploaded
3. The server then constructs an additional URL based upon the object
   URL, including information about the object size and type. A TTL is
   added to the URL, such that it will expire. The URL is then signed.
4. The server returns the object URL and the signed URL
5. The client submits a PUT request to the signed URL with the file
   contents.
6. If the PUT request succeeds, the client sends message stanza
   containing the link and additional metadata to whatever entity.

# Operator considerations

This module includes a `Content-Length` parameter in the upload URL;
however, it is the responsibility of the storage service to validate
this. Different storage services may behave differently or not respond
at all when a file is uploaded and the size does not exactly match. If
you intend to enforce a file size limit, make sure that your storage
service checks upload size against this parameter.

Furthermore, it is not the responsibility of this module to manage the
lifecycle of objects once uploaded. Not all services implement
lifecycle management or advanced features like tagging. To this end,
you might wish to configure an object lifecycle policy to control
costs, otherwise you might end up paying to store very old objects. To
this end, bear in mind that moving objects to a colder storage class
(if your service supports this) as part of a lifecycle policy could
generate considerable retrieval expenses - particularly when combined
combined with large MUCs.

# Known Working Services

This has been tested with the following services:

- **Wasabi** - which works very well. It is extremely cheap, but
  **does not support lifecycle management** or custom DNS.

It almost certainly works with Amazon S3.

# Configuration

The module expects a bucket URL, access key ID, secret, and region.

Furthermore, 

```yaml
modules:
  mod_s3_upload:
    # Required, characteristic values shown
    access_key_id: ABCDEF1234567890
    access_key_secret: whatever
    region: us-east-2
    bucket_url: https://my-bucket.whatever-service.com
    # Optional, defaults shown
    max_size: 1073741824
    put_ttl: 600
    set_public: true
    service_name: 'S3 Upload'
    access: local
    hosts:
      - upload.@HOST@
```
