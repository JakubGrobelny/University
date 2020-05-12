using System;
using System.IO;
using System.Net.Mail;
using System.Net.Mime;

namespace Zadanie1 {

    class SmtpFacade {
        public void Send(
            string from, 
            string to, 
            string subject, 
            string body, 
            Stream attachment, 
            string attachmentMimeType
        ) {
            MailMessage message = new MailMessage(from, to);
            message.Subject = subject;
            message.Body = body;

            if (attachment != null) {
                message.Attachments.Add(
                    new Attachment(attachment, attachmentMimeType)
                );
            }

            var client = new SmtpClient();
            client.UseDefaultCredentials = true;

            try {
                client.Send(message);
            } catch (Exception exc) {
                Console.Error.WriteLine(exc);
            }
        }
    }

    class Example {
        public static void Main() {
            
        }
    }
}

