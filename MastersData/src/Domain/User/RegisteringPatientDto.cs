namespace DDDSample1.Domain.User
{
    public class RegisteringPatientDto
    {
        public required string Name { get; set; }
        public required string PhoneNumber { get; set; }
        public required string Email { get; set; }
        public required string Password { get; set; }
    }
}