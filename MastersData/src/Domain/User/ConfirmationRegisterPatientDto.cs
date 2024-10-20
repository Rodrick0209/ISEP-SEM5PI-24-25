namespace DDDSample1.Domain.User
{
    public class ConfirmationRegisterPatientDto
    {
        public string Token { get; set; }
        public string Email { get; set; }

        public ConfirmationRegisterPatientDto(string token, string email)
        {
            this.Token = token;
            this.Email = email;
        }
    }
}