#nullable enable
namespace DDDSample1.Domain.User
{
    public class ConfirmationEditPatientSensitiveDataDto
    {
        public required string Token { get; set; }
        public required string Email { get; set; }
        public string? EmailToEdit { get; set; }
        public string? PhoneNumberToEdit { get; set; }
    }
}