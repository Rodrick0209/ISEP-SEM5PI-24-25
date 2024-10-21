#nullable enable
namespace DDDSample1.Domain.User
{
    public class EditingPatientDto
    {
        public required string Email { get; set; }
        public string? NameToEdit { get; set; }
        public string? EmailToEdit { get; set; }
        public string? PhoneNumberToEdit { get; set; }
    }
}