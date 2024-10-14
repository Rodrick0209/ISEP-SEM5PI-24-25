using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using DDDSample1.Infrastructure;

namespace DDDSample1.Domain.Patient
{
    public class PatientService : IPatientService
    {
        private readonly UnitOfWork _unitOfWork;
        private readonly IPatientRepository _patientRepository;

        public PatientService(UnitOfWork unitOfWork, IPatientRepository patientRepository)
        {
            _unitOfWork = unitOfWork;
            _patientRepository = patientRepository;
        }

        public async Task<PatientDto> CreateAsync(CreatingPatientProfileDto dto)
        {
            bool emailIsUnique = await ValidateEmailIsUnique(dto.Email);
            bool phoneNumberIsUnique = await ValidatePhoneNumberIsUnique(dto.PhoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email or Phone Number already exists");
            }

            var lastPatientInMonth = await _patientRepository.GetLastPatientInMonthAsync(DateTime.Now);

            var patient = new Patient(
                new FullName(dto.FullName),
                new DateOfBirth(DateTime.Parse(dto.DateOfBirth)),
                new Email(dto.Email),
                new PhoneNumber(dto.PhoneNumber),
                new MedicalRecordNumber(MedicalRecordNumberGenerator.GenerateMedicalRecordNumber(DateTime.Now, lastPatientInMonth)),
                new EmergencyContact(dto.EmergencyContact)
            );

            await _patientRepository.AddAsync(patient);

            await _unitOfWork.CommitAsync();

            return PatientMapper.ToDto(patient);
        }

        private async Task<bool> ValidateEmailIsUnique(string email)
        {
            var existingPatient = await _patientRepository.GetByEmailAsync(email);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> ValidatePhoneNumberIsUnique(string phoneNumber)
        {
            var existingPatient = await _patientRepository.GetByPhoneNumberAsync(phoneNumber);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }


    }
}
