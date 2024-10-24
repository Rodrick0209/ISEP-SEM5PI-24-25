namespace DDDSample1.Domain.User
{
    public class ConfirmationPatientDto
    {
        public string Token { get; set; }
        public string Email { get; set; }

        public ConfirmationPatientDto(string token, string email)
        {
            this.Token = token;
            this.Email = email;
        }
    }
}