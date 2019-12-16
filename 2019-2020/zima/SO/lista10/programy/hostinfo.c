#include "csapp.h"

int main(int argc, char* argv[]) {
    const char* service;
    const char* domain;

    if (argc == 2) {
        domain  = argv[1];
        service = NULL;
    } else if (argc == 3) {
        domain = argv[1];
        service = argv[2];
    } else {
        app_error("usage: %s <domain name> [service]\n", argv[0]);
    }

    /* Get a list of addrinfo records */
    struct addrinfo hints;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC; /* IPv4 only */
    hints.ai_socktype = SOCK_STREAM;

    /* Connections only */
    struct addrinfo* listp;
    int rc;
    if ((rc = getaddrinfo(domain, service, &hints, &listp)) != 0)
        gai_error(rc, "getaddrinfo");

    /* Walk the list and display each IP address */
    /* Display address string instead of domain name */
    int flags = NI_NUMERICHOST | NI_NUMERICSERV; 
    for (struct addrinfo* p = listp; p; p = p->ai_next) {
        char host_buf[MAXLINE];
        char serv_buf[MAXLINE];
        Getnameinfo(
            p->ai_addr, 
            p->ai_addrlen, 
            host_buf, MAXLINE, 
            serv_buf, MAXLINE, 
            flags
        );
        
        if (service && p->ai_family == AF_INET6) {
            printf("[%s]:%s\n", host_buf, serv_buf);
        } else if (service) {
            printf("%s:%s\n", host_buf, serv_buf);
        } else {
            printf("%s\n", host_buf);
        }
    }

    /* Clean up */
    freeaddrinfo(listp);

    return EXIT_SUCCESS;
}
