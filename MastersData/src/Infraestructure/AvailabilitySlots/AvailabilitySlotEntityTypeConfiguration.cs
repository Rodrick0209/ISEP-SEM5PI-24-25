using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.AvailabilitySlots;

namespace DDDSample1.Infrastructure.AvailabilitySlots
{
    internal class AvailabilitySlotEntityTypeConfiguration : IEntityTypeConfiguration<AvailabilitySlot>
    {
        public void Configure(EntityTypeBuilder<AvailabilitySlot> builder)
        {
            // Define a chave primária
            builder.HasKey(b => b.Id);

            // Configuração para a propriedade Date
            builder.OwnsOne(b => b.Date, date =>
            {
                date.Property(d => d.dateOfSlot)
                    .IsRequired() // Define como obrigatório
                    ; // Nome da coluna no banco de dados
            });

            // Configuração para StartTime e EndTime
            builder.Property(b => b.StartTime)
                   .IsRequired() // Define como obrigatório
                   ; 

            builder.Property(b => b.EndTime)
                   .IsRequired() // Define como obrigatório
                   ; 
        }
    }
}
