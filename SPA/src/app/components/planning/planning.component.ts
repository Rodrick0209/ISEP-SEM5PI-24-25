import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { PlanningService, Schedule, SurgeryRoom } from '../../services/planning.service';
import { Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './planning.component.html',
  styleUrls: ['./planning.component.css']
})
export class PlanningComponent implements OnInit {
  date: string = '';
  selectedSala: string = '';
  greetingMessage: string | null = null;
  salasDeCirurgia: any[] = []; // Atualizado para receber dados da API

  // Variável para a data mínima (hoje)
  today: string = '';
  scheduledData: Schedule | null = null; // Ajustado para um array de Schedule
  isLoading: boolean = false; // Controlar a animação de carregamento

  constructor(
    private planningService: PlanningService,
    private router: Router,
    private messageService: MessageService
  ) {}

  ngOnInit() {
    // Obtém a data atual no formato YYYY-MM-DD
    const currentDate = new Date();
    const year = currentDate.getFullYear();
    const month = (currentDate.getMonth() + 1).toString().padStart(2, '0');
    const day = currentDate.getDate().toString().padStart(2, '0');
    this.today = `${year}-${month}-${day}`;

    // Busca as salas de cirurgia da API
    this.getSurgeryRooms();
  }

  
  agendarCirurgia() {
    if (this.date && this.selectedSala) {
      this.isLoading = true; // Ativar o carregamento
      const formattedDate = this.date.replace(/-/g, ''); // Remove os hífens da data

      // Limpa os dados anteriores antes de tentar um novo agendamento
      this.scheduledData = null; 
      this.greetingMessage = 'Fetching schedule...'; 

      // Simulando o atraso na resposta da API com um setTimeout
        this.planningService.getScheduleFromPlanning(formattedDate, this.selectedSala).subscribe({
          next: (data) => {
            console.log('Schedule received from the Planning:', data);
            this.scheduledData = data;

            // Mensagem de sucesso
            this.messageService.setMessage('Schedule successfully received and updated!');
            this.router.navigate(['/planningResults'], { state: { schedule: data } });
            this.isLoading = false; // Desativar o carregamento

          },
          error: (err) => {
            console.error('Schedule not found, try other date or room', err);
            this.greetingMessage = 'Failed to fetch schedule.';
            this.isLoading = false; // Desativar o carregamento
          }
        });

    } else {
      this.greetingMessage = 'Please fill in all fields before scheduling.';
    }
  }

  getSurgeryRooms() {
    this.planningService.getSurgeryRooms().subscribe({
      next: (data: SurgeryRoom[]) => {
        console.log('Rooms received from the API:', data);
        this.salasDeCirurgia = data; // Atualiza as salas com os dados da API
      },
      error: (err: any) => {
        console.error('Failed to fetch surgery rooms', err);
      }
    });
  }

  closeScheduleBox() {
    this.scheduledData = null; // Fecha a div de detalhes do agendamento
  }

  fasterSolution() {
    this.greetingMessage = null;
    if (this.date && this.selectedSala) {
      this.isLoading = true; // Ativar o carregamento
      const formattedDate = this.date.replace(/-/g, ''); // Remove os hífens da data

      // Limpa os dados anteriores antes de tentar um novo agendamento
      this.scheduledData = null; 
      this.greetingMessage = 'Fetching schedule...'; 

      // Simulando o atraso na resposta da API com um setTimeout
        this.planningService.getHeuristicScheduleFromPlanning(formattedDate, this.selectedSala).subscribe({
          next: (data) => {
            console.log('Schedule received from the Planning:', data);
            this.scheduledData = data;

            // Mensagem de sucesso
            this.messageService.setMessage('Schedule successfully received and updated!');
            this.router.navigate(['/planningResults'], { state: { schedule: data } });
            this.isLoading = false; // Desativar o carregamento
          },
          error: (err) => {
            this.greetingMessage = 'Schedule not found, try other date or room';
            this.isLoading = false; // Desativar o carregamento
          } 
        });

    } else {
      this.greetingMessage = 'Please fill in all fields before scheduling.';
    }
  }


}
