namespace DDDSample1.Domain.Patient
{
    public class PatientMapper
    {
        public static PatientDto ToDto(Patient patient)
        {
            return new PatientDto(
                patient.Id.AsGuid(),
                patient.FullName.fullName,
                patient.DateOfBirth.dateOfBirth.ToString(),
                // patient.Email.email,
                patient.PhoneNumber.phoneNumber,
                patient.MedicalRecordNumber._medicalRecordNumber,
                patient.EmergencyContact.emergencyContact
            );
        }
    }
}
