
using System.Threading.Tasks;


namespace DDDSample1.Domain.User
{
    public interface IEmailSender
    {
               public Task SendEmailAsync(string body, string toEmail, string subject);

    }
}
