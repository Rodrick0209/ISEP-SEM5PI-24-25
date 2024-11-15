#nullable enable
using DDDSample1.Domain.Patients;

namespace DDDSample1.Domain.User
{
    public class ConfirmationEditPatientDto
    {
        public string? Token { get; set; }
        public string? Email { get; set; }
        public string? EmailToEdit { get; set; }
        public string? PhoneNumberToEdit { get; set; }

        public ConfirmationEditPatientDto() { }

        public ConfirmationEditPatientDto(string token, string email, string? emailToEdit, string? phoneNumberToEdit)
        {
            this.Token = token;
            this.Email = email;
            this.EmailToEdit = emailToEdit;
            this.PhoneNumberToEdit = phoneNumberToEdit;
        }
    }
}