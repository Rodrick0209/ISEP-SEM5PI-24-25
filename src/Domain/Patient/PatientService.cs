using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;
using DDDSample1.Infrastructure;
using Microsoft.EntityFrameworkCore.Storage;

namespace DDDSample1.Domain.Patient
{
    public class PatientService : IPatientService
    {
        private readonly UnitOfWork _unitOfWork;
        private readonly IPatientRepository _patientRepository;
        private readonly IEmailSender _emailSender;

        public PatientService(UnitOfWork unitOfWork, IPatientRepository patientRepository, IEmailSender emailSender)
        {
            _unitOfWork = unitOfWork;
            _patientRepository = patientRepository;
            _emailSender = emailSender;
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

        public async Task<PatientDto> UpdateAsync(EditingPatientProfileDto dto)
        {
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(dto.MedicalRecordNumber);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            string email = patient.Email.email;

            if (dto.FullName != null)
            {
                patient.ChangeFullName(new FullName(dto.FullName));
            }

            if (dto.Email != null)
            {
                bool emailIsUnique = await ValidateEmailIsUnique(dto.Email);
                if (!emailIsUnique)
                {
                    throw new BusinessRuleValidationException("Email already exists");
                }
                patient.ChangeEmail(new Email(dto.Email));
            }

            if (dto.PhoneNumber != null)
            {
                bool phoneNumberIsUnique = await ValidatePhoneNumberIsUnique(dto.PhoneNumber);
                if (!phoneNumberIsUnique)
                {
                    throw new BusinessRuleValidationException("Phone Number already exists");
                }
                patient.ChangePhoneNumber(new PhoneNumber(dto.PhoneNumber));
            }

            await _unitOfWork.CommitAsync();

            if (dto.Email != null || dto.PhoneNumber != null)
            {
                await _emailSender.SendEmailAsync("Your profile has been updated. If you did not make this change, please contact support immediately.", email, "Profile Update Notification");
            }

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
