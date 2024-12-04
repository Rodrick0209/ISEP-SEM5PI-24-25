import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { SpecializationService } from '../../services/specialization.service';
import { MessageService } from '../../services/message.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-create-specialization',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './create-specialization.component.html',
  styleUrl: './create-specialization.component.css'
})
export class CreateSpecializationComponent {

  newName: string = '';
  showConfirmation: boolean = false;
  errorMessage: string = '';



  constructor(private router: Router, private specialization: SpecializationService,private messageService: MessageService) { }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }


  onSubmit(newName: any): void {
    this.showConfirmation = false;
  
    if (newName.valid) {
      // Extraia o nome diretamente e formate para começar com letra maiúscula
      const name = this.capitalizeFirstLetter(newName.value.name);
  
      this.specialization.createSpecialization(name).subscribe({
        next: (data: any) => {
          this.messageService.setMessage('Specialization created successfully');
          this.router.navigate(['/specializations']);
        },
        error: (err: any) => {
          this.errorMessage = "Failed to create specialization";
        }
      });
    }
  }
  
  // Função para capitalizar a primeira letra
  private capitalizeFirstLetter(input: string): string {
    if (!input) return input; // Retorna vazio se a entrada for inválida
    return input.charAt(0).toUpperCase() + input.slice(1).toLowerCase();
  }
  



  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/specializations']);
  }


}
