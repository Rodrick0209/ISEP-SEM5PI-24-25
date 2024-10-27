using Microsoft.AspNetCore.Identity.UI.Services;
using MailKit.Net.Smtp; 
using MailKit.Security;
using MimeKit;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using System;
using Microsoft.Extensions.Configuration;


namespace DDDSample1.Domain.User
{
    public class EmailSender : IEmailSender
    {
        private readonly ILogger<EmailSender> _logger;
        private readonly IConfiguration _configuration;

        public EmailSender(ILogger<EmailSender> logger, IConfiguration configuration)
        {
            _logger = logger;
            _configuration = configuration;

        }

        public async Task SendEmailAsync(string body, string toEmail, string subject)
        {
            Console.WriteLine("Sending email...");
            var email = new MimeMessage();
            email.From.Add(MailboxAddress.Parse(_configuration["Gmail:AppEmail"]));
            email.To.Add(MailboxAddress.Parse(toEmail)); 
            email.Subject = subject;
            email.Body = new TextPart("html") { Text = body };
            using var smtp = new SmtpClient();
            
            smtp.Connect("smtp.gmail.com", 587, SecureSocketOptions.StartTls);
            
            smtp.Authenticate("projetolapr5grupo55@gmail.com", "cteyonyvmhzfhiug");
            await smtp.SendAsync(email);             smtp.Disconnect(true);
            _logger.LogInformation("Email enviado com sucesso para {Email}", toEmail);
        }
    }
}
