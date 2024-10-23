namespace DDDSample1.Domain.Patients
{
    public class PatientMapper
    {
        public static PatientDto ToDto(Patient patient)
        {
            return new PatientDto(
                patient.Id.AsGuid(),
                patient.FullName.fullName,
                patient.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd"),
                patient.Gender.gender,
                patient.Email.email,
                patient.PhoneNumber.phoneNumber,
                patient.MedicalRecordNumber._medicalRecordNumber,
                new EmergencyContactDto(
                    patient.EmergencyContact.Name.fullName,
                    patient.EmergencyContact.Email.email,
                    patient.EmergencyContact.PhoneNumber.phoneNumber
                ),
                patient.MedicalConditions?.medicalConditions ?? null
            );
        }
    }
}
