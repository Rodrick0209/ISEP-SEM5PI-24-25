using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Appointments;

namespace DDDSample1.Infrastructure.Appointments
{
    internal class AppointmentEntityTypeConfiguration : IEntityTypeConfiguration<Appointment>
    {
        public void Configure(EntityTypeBuilder<Appointment> builder)
        {


            // Definindo a chave primária
            builder.HasKey(a => a.Id);

            // Configurando AppointmentTimeSlot
            builder.OwnsOne(a => a.AppointmentTimeSlot, ats =>
            {
                // Configurando a propriedade Date
                ats.WithOwner();
                ats.Property(a => a.Date).IsRequired();

                // Configurando TimeSlot (StartMinute e EndMinute)
                ats.OwnsOne(a => a.TimeSlot, ts =>
                {
                    ts.Property(t => t.StartMinute)
                        .IsRequired();

                    ts.Property(t => t.EndMinute)
                        .IsRequired();
                });
            });


        }
    }
}

