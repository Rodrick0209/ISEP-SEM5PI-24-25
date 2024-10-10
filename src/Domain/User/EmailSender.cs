using Microsoft.AspNetCore.Identity.UI.Services;
using MailKit.Net.Smtp; 
using MailKit.Security;
using MimeKit;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using System;

namespace DDDSample1.Domain.User
{
    public class EmailSender : IEmailSender
    {
        private readonly ILogger<EmailSender> _logger;

        public EmailSender(ILogger<EmailSender> logger)
        {
            _logger = logger;
        }

        public async Task SendEmailAsync(string body, string toEmail, string subject)
        {
            Console.WriteLine("Sending email...");
            var email = new MimeMessage();
            email.From.Add(MailboxAddress.Parse("tremaine86@ethereal.email"));
            email.To.Add(MailboxAddress.Parse("tremaine86@ethereal.email")); 
            email.Subject = subject;
            email.Body = new TextPart("html") { Text = body };
            using var smtp = new SmtpClient();
            
            //aqui temos de trocar o connect se quisermos utilizar o gmail ou outro para ja este para o ethereal que é uma plataforma de teste de emails
            smtp.Connect("smtp.ethereal.email", 587, SecureSocketOptions.StartTls);
            smtp.Authenticate("tremaine86@ethereal.email", "jMttkxreDK322Sxr2M");
            await smtp.SendAsync(email);             smtp.Disconnect(true);
            Console.WriteLine("Email sent supostamente!");
            _logger.LogInformation("Email enviado com sucesso para {Email}", toEmail);
        }
    }
}
